# Granola

A Redis-based ordered autocomplete service.

## Requirements

* [stack](https://github.com/commercialhaskell/stack)
* Redis

## Testing

**Warning**: the tests will attempt to connect to Redis at localhost:6379 and
overwrite (flush) the 16th database.

You might want to run this command in an isolated container.

```
stack test
```
