use serde::{Deserialize, Serialize};
use options::FilterOptions;

#[derive(Serialize, Deserialize, PartialEq, Clone)]
pub struct RawSerializedFilter {
    pub payload: Vec<u8>,
    pub num_bits: u64,
    pub num_funs: u32,
    pub sip00: u64,
    pub sip01: u64,
    pub sip10: u64,
    pub sip11: u64,
}

#[derive(Serialize, Deserialize, PartialEq, Clone)]
pub struct SerializedFilter {
    pub filters: Vec<RawSerializedFilter>,
    pub opts: FilterOptions,
    pub upsert_num: u64,
}
