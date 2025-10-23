use std::mem::size_of;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::collections::hash_map::DefaultHasher;

fn slice_index_from_vec<T>(vec: &[T], slice: &[T]) -> Option<usize> {
    let vec_start = vec.as_ptr() as usize;
    let slice_start = slice.as_ptr() as usize;

    // Check if slice is within vec
    if slice_start >= vec_start && slice_start <= vec_start + std::mem::size_of_val(vec) {
        let offset_bytes = slice_start - vec_start;
        Some(offset_bytes / size_of::<T>())
    } else {
        None
    }
}

/// Hash Consing Container - A generic data structure that uses hash-based memoization
/// to store objects and detect duplicates. Each push operation returns a hash that
/// serves as a pointer to the object, enabling O(1) access and deduplication.
/// Type-safe hash pointer for HashCons
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HashPtr<T> {
    hash: [u8; 16],
    _marker: std::marker::PhantomData<T>,
}

impl<T> HashPtr<T> {
    pub fn as_bytes(&self) -> &[u8; 16] {
        &self.hash
    }
}

#[derive(Debug, Clone)]
pub struct HashCons<T: Eq + Hash + Clone> {
    /// Maps hash values to their corresponding objects
    storage: HashMap<u64, T>,
}

impl<T: Eq + Hash + Clone> HashCons<T> {
    /// Creates a new empty HashCons container
    pub fn new() -> Self {
        HashCons {
            storage: HashMap::new(),
        }
    }

    /// Pushes an object into the container and returns its hash.
    /// If the object already exists, returns the existing hash (deduplication).
    pub fn push(&mut self, obj: T) -> u64 {
        let mut hasher = DefaultHasher::new();
        obj.hash(&mut hasher);
        let hash = hasher.finish();

        if !self.storage.contains_key(&hash) {
            self.storage.insert(hash, obj);
        }
        hash
    }

    /// Retrieves an object by its hash. Returns None if not found.
    pub fn get(&self, hash: u64) -> Option<&T> {
        self.storage.get(&hash)
    }

    /// Retrieves a mutable reference to an object by its hash.
    pub fn get_mut(&mut self, hash: u64) -> Option<&mut T> {
        self.storage.get_mut(&hash)
    }

    /// Checks if an object with the given hash exists in the container
    pub fn contains(&self, hash: u64) -> bool {
        self.storage.contains_key(&hash)
    }

    /// Returns the number of unique objects in the container
    pub fn len(&self) -> usize {
        self.storage.len()
    }

    /// Checks if the container is empty
    pub fn is_empty(&self) -> bool {
        self.storage.is_empty()
    }

    /// Clears all objects from the container
    pub fn clear(&mut self) {
        self.storage.clear();
    }
}

impl<T: Eq + Hash + Clone> Default for HashCons<T> {
    fn default() -> Self {
        Self::new()
    }
}

