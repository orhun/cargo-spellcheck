//! Configure cargo-spellcheck
//!
//! Supports `Hunspell` and `LanguageTool` scopes.
//!
//! A default configuration will be generated in the default
//! location by default. Default. Default default default.

use crate::suggestion::Detector;
use crate::wrap::WrapConfig;
use anyhow::{anyhow, bail, Error, Result};
use fancy_regex::Regex;
use log::trace;
use serde::{Deserialize, Serialize};
use std::convert::AsRef;
use std::fmt;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::path::{Path, PathBuf};

#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct Config {
    #[serde(rename = "Hunspell")]
    pub hunspell: Option<HunspellConfig>,
    #[serde(rename = "LanguageTool")]
    pub languagetool: Option<LanguageToolConfig>,
    wrapper: Option<WrapConfig>,
}

#[derive(Debug)]
pub struct WrappedRegex(pub Regex);

impl Clone for WrappedRegex {
    fn clone(&self) -> Self {
        // @todo inefficient.. but right now this should almost never happen
        // @todo implement a lazy static `Arc<Mutex<HashMap<&'static str,Regex>>`
        Self(Regex::new(self.as_str()).unwrap())
    }
}

impl std::ops::Deref for WrappedRegex {
    type Target = Regex;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::convert::AsRef<Regex> for WrappedRegex {
    fn as_ref(&self) -> &Regex {
        &self.0
    }
}

impl Serialize for WrappedRegex {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::ser::Serializer,
    {
        serializer.serialize_str(self.as_str())
    }
}

impl<'de> Deserialize<'de> for WrappedRegex {
    fn deserialize<D>(deserializer: D) -> Result<WrappedRegex, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        deserializer
            .deserialize_any(RegexVisitor)
            .map(WrappedRegex::from)
    }
}

impl Into<Regex> for WrappedRegex {
    fn into(self) -> Regex {
        self.0
    }
}

impl From<Regex> for WrappedRegex {
    fn from(other: Regex) -> WrappedRegex {
        WrappedRegex(other)
    }
}

struct RegexVisitor;

impl<'de> serde::de::Visitor<'de> for RegexVisitor {
    type Value = Regex;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("String with valid regex expression")
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        let re = Regex::new(value).map_err(E::custom)?;
        Ok(re)
    }

    fn visit_string<E>(self, value: String) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.visit_str::<E>(value.as_str())
    }
}

#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct Quirks {
    /// A regular expression, whose capture groups will be checked, instead of the initial token.
    /// Only the first one that matches will be used to split the word.
    pub transform_regex: Option<Vec<WrappedRegex>>,
    /// Allow concatenated words instead of dashed connection.
    /// Note that this only applies, if one of the suggested replacements has an item that is
    /// equivalent except for addition dashes (`-`).
    pub allow_concatenation: Option<bool>,
}

impl Default for Quirks {
    fn default() -> Self {
        // use some for default, so for generating the default config has the default values
        // but the options are necessary to allow omitting them in the config file
        Self {
            transform_regex: Some(vec![]),
            allow_concatenation: Some(false),
        }
    }
}

impl Quirks {
    pub(crate) fn allow_concatenated(&self) -> bool {
        self.allow_concatenation.unwrap_or(false)
    }

    pub(crate) fn transform_regex(&self) -> &[WrappedRegex] {
        if let Some(ref tr) = self.transform_regex {
            tr.as_slice()
        } else {
            &[]
        }
    }
}

#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct HunspellConfig {
    /// The language we want to check against, used as the dictionary and affixes file name.
    // TODO impl a custom xx_YY code deserializer based on iso crates
    pub lang: Option<String>,
    /// Addition search dirs for `.dic` and `.aff` files.
    // must be option so it can be omitted in the config
    pub search_dirs: Option<Vec<PathBuf>>,
    /// Additional dictionaries for topic specific lingo.
    pub extra_dictonaries: Option<Vec<PathBuf>>,
    /// Additional quirks besides dictionary lookups.
    pub quirks: Option<Quirks>,
}

impl HunspellConfig {
    pub fn lang(&self) -> &str {
        if let Some(ref lang) = self.lang {
            lang.as_str()
        } else {
            "en_US"
        }
    }

    pub fn search_dirs(&self) -> &[PathBuf] {
        if let Some(ref search_dirs) = &self.search_dirs {
            search_dirs.as_slice()
        } else {
            lazy_static::lazy_static! {
                static ref LOOKUP_DIRS: Vec<PathBuf> = vec![PathBuf::from("/usr/share/myspell")];
            };

            LOOKUP_DIRS.as_slice()
        }
    }

    pub fn extra_dictonaries(&self) -> &[PathBuf] {
        if let Some(ref extra_dictonaries) = self.extra_dictonaries {
            extra_dictonaries.as_slice()
        } else {
            &[]
        }
    }

    pub fn sanitize_paths(&mut self, base: &Path) -> Result<()> {
        if let Some(ref mut search_dirs) = &mut self.search_dirs {
            for path in search_dirs.iter_mut() {
                let abspath = if !path.is_absolute() {
                    base.join(path.clone())
                } else {
                    path.to_owned()
                };
                let abspath = std::fs::canonicalize(abspath)?;
                trace!(
                    "Sanitized ({} + {}) -> {}",
                    base.display(),
                    path.display(),
                    abspath.display()
                );
                *path = abspath;
            }
        }
        Ok(())
    }
}

#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct LanguageToolConfig {
    pub url: url::Url,
}

impl LanguageToolConfig {
    pub fn url(&self) -> &url::Url {
        &self.url
    }
}

impl Config {
    const QUALIFIER: &'static str = "io";
    const ORGANIZATION: &'static str = "spearow";
    const APPLICATION: &'static str = "cargo_spellcheck";

    /// Sanitize all relative paths to absolute paths
    /// in relation to `base`.
    fn sanitize_paths(&mut self, base: &Path) -> Result<()> {
        if let Some(ref mut hunspell) = self.hunspell {
            hunspell.sanitize_paths(base)?;
        }
        Ok(())
    }

    pub fn parse<S: AsRef<str>>(s: S) -> Result<Self> {
        Ok(toml::from_str(s.as_ref())?)
    }

    pub fn load_from<P: AsRef<Path>>(path: P) -> Result<Self> {
        let mut file = File::open(path.as_ref().to_str().unwrap())
            .map_err(|e| anyhow!("Failed to open file {}", path.as_ref().display()).context(e))?;
        let mut contents = String::with_capacity(1024);
        file.read_to_string(&mut contents).map_err(|e| {
            anyhow!("Failed to read from file {}", path.as_ref().display()).context(e)
        })?;
        Self::parse(&contents)
            .map_err(|e| {
                e.context(anyhow::anyhow!(
                    "Syntax of a given config file({}) is broken",
                    path.as_ref().display()
                ))
            })
            .and_then(|mut cfg| {
                if let Some(base) = path.as_ref().parent() {
                    cfg.sanitize_paths(base)?;
                }
                Ok(cfg)
            })
    }

    pub fn load() -> Result<Self> {
        if let Some(base) = directories::BaseDirs::new() {
            Self::load_from(
                base.config_dir()
                    .join("cargo_spellcheck")
                    .join("config.toml"),
            )
        } else {
            bail!("No idea where your config directory is located. XDG compliance would be nice.")
        }
    }

    pub fn to_toml(&self) -> Result<String> {
        toml::to_string(self).map_err(|e| anyhow!("Failed to convert to toml").context(e))
    }

    pub fn write_values_to_path<P: AsRef<Path>>(&self, path: P) -> Result<Self> {
        let s = self.to_toml()?;
        let path = path.as_ref();

        if let Some(path) = path.parent() {
            std::fs::create_dir_all(path).map_err(|e| {
                anyhow!("Failed to create directories {}", path.display()).context(e)
            })?;
        }

        let file = std::fs::OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(path)
            .map_err(|e| {
                anyhow!("Failed to write default values to {}", path.display()).context(e)
            })?;
        let mut writer = std::io::BufWriter::new(file);

        writer.write_all(s.as_bytes()).map_err(|e| {
            anyhow!("Failed to write default config to {}", path.display()).context(e)
        })?;

        Ok(self.clone())
    }

    pub fn write_values_to_default_path(&self) -> Result<Self> {
        let path = Self::default_path()?;
        self.write_values_to_path(path)
    }

    pub fn write_default_values_to<P: AsRef<Path>>(path: P) -> Result<Self> {
        Self::default().write_values_to_path(path)
    }

    pub fn default_path() -> Result<PathBuf> {
        if let Some(base) =
            directories::ProjectDirs::from(Self::QUALIFIER, Self::ORGANIZATION, Self::APPLICATION)
        {
            Ok(base.config_dir().join("config.toml"))
        } else {
            bail!("No idea where your config directory is located. `$HOME` must be set.")
        }
    }

    pub fn write_default_values() -> Result<Self> {
        let d = Self::default_path()?;
        Self::write_default_values_to(d.join("config.toml"))
    }

    pub fn is_enabled(&self, detector: Detector) -> bool {
        match detector {
            Detector::Hunspell => self.hunspell.is_some(),
            Detector::LanguageTool => self.languagetool.is_some(),
            Detector::Wrapper => self.wrapper.is_some(),
            #[cfg(test)]
            Detector::Dummy => true,
        }
    }

    pub fn full() -> Self {
        let languagetool = LanguageToolConfig {
            url: url::Url::parse("http://127.0.0.1:8010").expect("Default ip must be ok"),
        };
        Self {
            languagetool: Some(languagetool),
            ..Default::default()
        }
    }
}

impl Default for Config {
    fn default() -> Self {
        let mut search_dirs = if cfg!(target_os = "macos") {
            directories::BaseDirs::new()
                .map(|base| vec![base.home_dir().to_owned().join("/Library/Spelling/")])
                .unwrap_or_else(|| Vec::with_capacity(2))
        } else {
            Vec::with_capacity(2)
        };

        #[cfg(target_os = "macos")]
        search_dirs.push(PathBuf::from("/Library/Spelling/"));

        #[cfg(target_os = "linux")]
        search_dirs.extend(vec![
            // Fedora
            PathBuf::from("/usr/share/myspell/"),
            // Arch Linux
            PathBuf::from("/usr/share/hunspell/"),
            PathBuf::from("/usr/share/myspell/dicts/"),
        ]);

        Self {
            hunspell: Some(HunspellConfig {
                lang: Some("en_US".to_owned()),
                search_dirs: Some(search_dirs),
                extra_dictonaries: Some(Vec::new()),
                quirks: Some(Quirks::default()),
            }),
            languagetool: None,
            wrapper: None,
        }
    }
}

// @todo figure out which ISO spec this actually is
pub struct CommonLang(String);

impl std::str::FromStr for CommonLang {
    type Err = Error;
    fn from_str(_s: &str) -> std::result::Result<Self, Self::Err> {
        //
        unimplemented!("Common Lang needs a ref spec")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn all() {
        let _ = Config::parse(
            r#"
[LanguageTool]
url = "http://127.0.0.1:8010/"

[Hunspell]
lang = "en_US"
search_dirs = ["/usr/lib64/hunspell"]
extra_dictonaries = ["/home/bernhard/test.dic"]
			"#,
        )
        .unwrap();
    }

    #[test]
    fn empty() {
        let _ = Config::parse(
            r#"
			"#,
        )
        .unwrap();
    }
    #[test]
    fn partial_1() {
        let _ = Config::parse(
            r#"
[hunspell]
lang = "en_US"
search_dirs = ["/usr/lib64/hunspell"]
extra_dictonaries = ["/home/bernhard/test.dic"]
			"#,
        )
        .unwrap();
    }

    #[test]
    fn partial_2() {
        let _ = Config::parse(
            r#"
[languageTool]

[Hunspell]
lang = "en_US"
search_dirs = ["/usr/lib64/hunspell"]
extra_dictonaries = ["/home/bernhard/test.dic"]
			"#,
        )
        .unwrap();
    }

    #[test]
    fn partial_3() {
        let _ = Config::parse(
            r#"
[Hunspell]
lang = "en_US"
search_dirs = ["/usr/lib64/hunspell"]
extra_dictonaries = ["/home/bernhard/test.dic"]
			"#,
        )
        .unwrap();
    }

    #[test]
    fn partial_4() {
        let _ = Config::parse(
            r#"
[LanguageTool]
url = "http://127.0.0.1:8010/"

[Hunspell]
lang = "en_US"
			"#,
        )
        .unwrap();
    }

    #[test]
    fn partial_5() {
        let _ = Config::parse(
            r#"
[hUNspell]
lang = "en_US"
			"#,
        )
        .unwrap();
    }

    #[test]
    fn partial_6() {
        let _ = Config::parse(
            r#"
[hunspell]
			"#,
        )
        .unwrap();
    }

    #[test]
    fn partial_7() {
        let _ = Config::parse(
            r#"
[hunspell]
[hunspell.quirks]
allow_concatenation = true
transform_regex = ["^'([^\\s])'$", "^[0-9]+x$"]
			"#,
        )
        .unwrap();
    }
}
