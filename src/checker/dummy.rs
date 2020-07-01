use super::tokenize;
use super::Checker;
use crate::documentation::Documentation;
use crate::suggestion::{Detector, Suggestion, SuggestionSet};
use anyhow::Result;
use log::trace;
use std::path::PathBuf;

/// A test checker that tokenizes and marks everything as wrong
pub struct DummyChecker;

impl Checker for DummyChecker {
    type Config = ();

    fn check<'a, 's>(docu: &'a Documentation, _: &Self::Config) -> Result<SuggestionSet<'s>>
    where
        'a: 's,
    {
        let suggestions = docu.iter().try_fold::<SuggestionSet, _, Result<_>>(
            SuggestionSet::new(),
            |mut acc, (origin, chunks)| {
                let chunk = chunks.iter().next().unwrap();
                let plain = chunk.erase_markdown();
                for (index, range) in dbg!(tokenize(plain.as_str())).into_iter().enumerate() {
                    trace!("Token: >{}<", &plain.as_str()[range.clone()]);
                    let detector = Detector::Dummy;
                    let spans = plain.find_spans(dbg!(range.clone()));
                    assert_eq!(dbg!(&spans).len(), 1);
                    for (range, span) in spans {
                        trace!("Suggestion for {:?} -> {}", range, chunk.display(range.clone()));
                        let replacements = vec![format!("replacement_{}", index)];
                        let suggestion = Suggestion {
                            detector,
                            span,
                            origin: origin.clone(),
                            replacements,
                            chunk,
                            description: None,
                        };
                        acc.add(origin.clone(), suggestion);
                    }
                }
                Ok(acc)
            },
        )?;

        Ok(suggestions)
    }
}