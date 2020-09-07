mod bloom;
mod forgetful;

pub use self::{bloom::BloomFilter, forgetful::ForgetfulFilter};
use crate::{container::SerializedFilter, options::FilterOptions};
use rustler::{Atom, Decoder, Encoder, Env, Error, NifResult, Term};
use serde::{Deserialize, Serialize};

type Result<T> = std::result::Result<T, String>;

pub enum Filter {
    Bloom(BloomFilter),
    Forgetful(ForgetfulFilter),
}

impl Filter {
    pub fn new(opts: FilterOptions) -> Result<Self> {
        Ok(match opts.filter_type.ok_or("`filter_type` not set")? {
            FilterType::Forgetful => Filter::Forgetful(ForgetfulFilter::new(opts)?),
            FilterType::Bloom => Filter::Bloom(BloomFilter::new(opts)?),
        })
    }

    pub fn filter_type(&self) -> FilterType {
        match self {
            Self::Bloom(_) => FilterType::Bloom,
            Self::Forgetful(_) => FilterType::Forgetful,
        }
    }

    pub fn serialize(&self) -> Result<Vec<u8>> {
        match self {
            Self::Bloom(filt) => filt.serialize(),
            Self::Forgetful(filt) => filt.serialize(),
        }
    }

    pub fn restore(prev_filter: SerializedFilter) -> Result<Self> {
        Ok(
            match prev_filter
                .opts
                .filter_type
                .ok_or("`filter_type` not set")?
            {
                FilterType::Bloom => Self::Bloom(BloomFilter::restore(prev_filter)),
                FilterType::Forgetful => Self::Forgetful(ForgetfulFilter::restore(prev_filter)),
            },
        )
    }

    pub fn clear(&mut self) {
        match self {
            Self::Bloom(filt) => filt.clear(),
            Self::Forgetful(filt) => filt.clear(),
        }
    }

    pub fn check(&self, key: &[u8]) -> bool {
        match self {
            Self::Bloom(filt) => filt.check(key),
            Self::Forgetful(filt) => filt.check(key),
        }
    }
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Clone, Copy)]
pub enum FilterType {
    Bloom,
    Forgetful,
}

impl<'a> Decoder<'a> for FilterType {
    fn decode(term: Term<'a>) -> NifResult<Self> {
        use crate::atoms;
        let ft = Atom::from_term(term)?;
        if ft == atoms::bloom() {
            Ok(Self::Bloom)
        } else if ft == atoms::fbf() {
            Ok(Self::Forgetful)
        } else {
            Err(Error::RaiseTerm(Box::new(atoms::wrong_filter_type())))
        }
    }
}

impl Encoder for FilterType {
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        match self {
            Self::Bloom => crate::atoms::bloom(),
            Self::Forgetful => crate::atoms::fbf(),
        }
        .encode(env)
    }
}
