use std::error;

use bloomfilter::Bloom;

use container::{FilterType, RawSerializedFilter, SerializedFilter};
use options::FilterOptions;
use siphasher::sip::SipHasher13;
use std::hash::Hash;
use std::hash::Hasher;

type Result<T> = std::result::Result<T, Box<dyn error::Error>>;

pub struct BloomFilter {
    pub filter: Bloom<[u8]>,
    pub bitmap_size: usize,
    pub items_count: usize,
    pub fp_rate: f64,
}

impl BloomFilter {
    pub fn new(opts: FilterOptions) -> BloomFilter {
        BloomFilter {
            filter: if opts.items_count > 0 && opts.bitmap_size == 0 {
                Bloom::new_for_fp_rate(opts.items_count as usize, opts.fp_rate)
            } else {
                Bloom::new(opts.bitmap_size, opts.items_count)
            },
            bitmap_size: opts.bitmap_size,
            items_count: opts.items_count,
            fp_rate: opts.fp_rate,
        }
    }

    pub fn set(&mut self, key: &Vec<u8>) {
        self.filter.set(key)
    }

    pub fn check(&self, key: &Vec<u8>) -> bool {
        self.filter.check(key)
    }

    pub fn check_serialized(&self, filter: SerializedFilter, key: &Vec<u8>) -> bool {
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

    pub fn check_and_set(&mut self, key: &Vec<u8>) -> bool {
        self.filter.check_and_set(key)
    }

    pub fn clear(&mut self) {
        self.filter.clear();
    }

    pub fn serialize(&self) -> Result<Vec<u8>> {
        let mut opts = FilterOptions::default();
        opts.filter_type = FilterType::Bloom;
        opts.bitmap_size = self.bitmap_size;
        opts.items_count = self.items_count;
        opts.fp_rate = self.fp_rate;

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
            opts: opts,
            upsert_num: 0,
        }) {
            Ok(res) => Ok(res),
            Err(e) => Err(e)
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
            bitmap_size: prev_filter.opts.bitmap_size,
            items_count: prev_filter.opts.items_count,
            fp_rate: prev_filter.opts.fp_rate,
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