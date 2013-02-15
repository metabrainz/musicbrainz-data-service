# `musicbrainz-data-service`

A JSON-over-HTTP service to perform operations provided by
[`musicbrainz-data`](http://github.com/metabrainz/musicbrainz-data.git).

# Cloning

`musicbrainz-data-service` currently uses Git submodules to pin the required
version of `musicbrainz-data` that it wraps. It's recommend to clone this
repository with the `--recursive` flag:

    git clone --recursive git://github.com/metabrainz/musicbrainz-data-service.git

# Installing

`musicbrainz-data-service` is packaged using Cabal, and requires GHC 7.4 (or
higher). We recommend installing the 2012.2.0.0 version of the
[Haskell Platform](http://haskell.org/platform), as this provides a lot of
dependencies this project uses; however, with an up to date `cabal-install` and
the aforementioned GHC version satisfied you should also be able to build this
project.

You also need access to a PostgreSQL database server with the `cube` and
`uuid-ossp` extensions, on Ubuntu 12.04 and 12.10 these extensions are in the
postgresql-contrib-9.1 package.

Before you can build `musicbrainz-data-service`, you will need to install
`musicbrainz-data`. To do so, run the following commands from inside your
`musicbrainz-data-service` checkout:

    cd musicbrainz-data
    cabal install
    cd ..

Once you have those requirements, you can now run:

    cabal install

# Working on `musicbrainz-data-service`.

## Dependencies

To install just the dependencies needed to run `musicbrainz-data-service`, run:

`cabal install --only-dependencies`.

If you plan to run tests, you will need to run:

`cabal install --enable-tests --only-dependencies`.

## Building

To build, simply run `cabal build`.

## Running tests

To run tests you first need to set up the test database, run the
following commands (obviously the createuser isn't needed if you
already have a musicbrainz user, and the dropdb isn't needed if you
don't have an existing musicbrainz_nes database):

    createuser -U postgres musicbrainz --no-createdb --no-superuser --no-createrole;
    dropdb     -U postgres musicbrainz_nes
    createdb   -U postgres musicbrainz_nes --owner=musicbrainz
    psql       -U postgres musicbrainz_nes

That last command will start the postgresql prompt, on that prompt run
the following commands:

    CREATE EXTENSION "uuid-ossp"
    CREATE EXTENSION cube
    \i musicbrainz-data/test/data/test-schema.sql
    \q

Your database is now ready to run the tests.

We use Cabal to run tests. To build tests while you develop, enable test
building when you configure musicbrainz-data:

`cabal configure --enable-tests`

Every time you run `cabal build` you will also build tests. You can run tests by
running:

`cabal test`

Check the output of `cabal test --help` for various things that can also be done
while you run tests.
