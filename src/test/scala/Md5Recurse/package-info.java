package Md5Recurse;

// Missing tests
// Symlinks: Don't follow symlinks pointing to dirs. Esp importantent with circular links like 'link -> .' or 'link->/'. On Windows it seems administrator is needed to create symlinks, so that
// makes it difficult to test.