use super::FilterType;
use bloomfilter::Bloom;
use container::{RawSerializedFilter, SerializedFilter};
use options::FilterOptions;
use siphasher::sip::SipHasher13;
use std::hash::Hash;
use std::hash::Hasher;

type Result<T> = std::result::Result<T, String>;

pub struct BloomFilter {
    pub filter: Bloom<[u8]>,
}

impl BloomFilter {
    pub fn new(opts: FilterOptions) -> Result<BloomFilter> {
        assert_eq!(opts.filter_type, Some(FilterType::Bloom));
        Ok(BloomFilter {
            filter: match opts {
                FilterOptions {
                    bitmap_size: None,
                    items_count: Some(items_count),
                    fp_rate: Some(fp_rate),
                    ..
                } => Bloom::new_for_fp_rate(items_count, fp_rate),
                FilterOptions {
                    bitmap_size: Some(bitmap_size),
                    items_count: Some(items_count),
                    ..
                } => Bloom::new(bitmap_size, items_count),
                _ => {
                    return Err(format!(
                        "must set `items_count` AND (`fp_rate` OR `bitmap_size`)], got {:?}",
                        opts
                    ))
                }
            },
        })
    }

    pub fn set(&mut self, key: &[u8]) {
        self.filter.set(key)
    }

    pub fn check(&self, key: &[u8]) -> bool {
        self.filter.check(key)
    }

    pub fn check_serialized(&self, filter: SerializedFilter, key: &[u8]) -> bool {
        let pf = &filter.filters.to_vec()[0];
        let sips = [
            SipHasher13::new_with_keys(pf.sip00, pf.sip01),
            SipHasher13::new_with_keys(pf.sip10, pf.sip11),
        ];
        let mut hashes = [0u64, 0u64];
        for k_i in 0..pf.num_funs {
            let bit_offset = (bloom_hash(&mut hashes, &key, k_i, &sips) % pf.num_bits) as usize;
            let byte_offset = bit_offset / 8;
            let bit = 7 - (bit_offset % 8);
            if (pf.payload[byte_offset] >> bit) & 1 != 1 {
                return false;
            }
        }
        true
    }

    pub fn check_and_set(&mut self, key: &[u8]) -> bool {
        self.filter.check_and_set(key)
    }

    pub fn clear(&mut self) {
        self.filter.clear();
    }

    pub fn serialize(&self) -> Result<Vec<u8>> {
        let mut opts = FilterOptions::default();
        opts.filter_type = Some(FilterType::Bloom);

        let sips = self.filter.sip_keys();
        let bitmap = self.filter.bitmap();
        match bincode::serialize(&SerializedFilter {
            filters: vec![RawSerializedFilter {
                payload: bitmap,
                num_bits: self.filter.number_of_bits(),
                num_funs: self.filter.number_of_hash_functions(),
                sip00: sips[0].0,
                sip01: sips[0].1,
                sip10: sips[1].0,
                sip11: sips[1].1,
            }],
            opts,
            upsert_num: 0,
        }) {
            Ok(res) => Ok(res),
            Err(e) => Err(format!("bincode serialization failed with: {}", e)),
        }
    }

    pub fn restore(prev_filter: SerializedFilter) -> BloomFilter {
        let pf = &prev_filter.filters.to_vec()[0];

        BloomFilter {
            filter: Bloom::from_existing(
                &pf.payload,
                pf.num_bits,
                pf.num_funs,
                [(pf.sip00, pf.sip01), (pf.sip10, pf.sip11)],
            ),
        }
    }
}

// helper for check_serialized, extracted from the bloom crate source code
fn bloom_hash(hashes: &mut [u64; 2], item: &[u8], k_i: u32, sips: &[SipHasher13; 2]) -> u64 {
    if k_i < 2 {
        let mut sip = sips[k_i as usize];
        item.hash(&mut sip);
        let hash = sip.finish();
        hashes[k_i as usize] = hash;
        hash
    } else {
        hashes[0].wrapping_add((u64::from(k_i)).wrapping_mul(hashes[1]) % 0xffff_ffff_ffff_ffc5)
    }
}
