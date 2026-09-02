# scalabridge - a bridge game core written in scala. Immutable and (mostly) free of side effects 

## About the project

scalabridge is used by [libridge.club](https://libridge.club/).

## Scala version

scalabridge uses scala 3.

## Compiling

scalabridge uses sbt to build. The following line should be enough:

```
sbt package
```
## Testing

Run tests: `sbt test`

Force run all tests: `sbt testFull`

Unit tests: `sbt 'testOnly *Test'`

Acceptance tests: `sbt 'testOnly *Feature'`


## Authors and copyright

### Authors:
See file AUTHORS

### Copyright information:
See file COPYRIGHT

### Full license text:
See file COPYING
