mod bloom;
mod forgetful;

pub use self::{bloom::BloomFilter, forgetful::ForgetfulFilter};
use crate::{container::SerializedFilter, options::FilterOptions};
use serde::{Deserialize, Serialize};

pub enum Filter {
    Bloom(BloomFilter),
    Forgetful(ForgetfulFilter),
}

impl Filter {
    pub fn new(opts: FilterOptions) -> Self {
        match opts.filter_type {
            FilterType::Forgetful => Filter::Forgetful(ForgetfulFilter::new(opts)),
            FilterType::Bloom => Filter::Bloom(BloomFilter::new(opts)),
        }
    }

    pub fn filter_type(&self) -> FilterType {
        match self {
            Self::Bloom(_) => FilterType::Bloom,
            Self::Forgetful(_) => FilterType::Forgetful,
        }
    }

    pub fn serialize(&self) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
        match self {
            Self::Bloom(filt) => filt.serialize(),
            Self::Forgetful(filt) => filt.serialize(),
        }
    }

    pub fn restore(prev_filter: SerializedFilter) -> Self {
        match prev_filter.opts.filter_type {
            FilterType::Bloom => Self::Bloom(BloomFilter::restore(prev_filter)),
            FilterType::Forgetful => Self::Forgetful(ForgetfulFilter::restore(prev_filter)),
        }
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

#[derive(Serialize, Deserialize, PartialEq, Clone, Copy)]
pub enum FilterType {
    Bloom,
    Forgetful,
}
