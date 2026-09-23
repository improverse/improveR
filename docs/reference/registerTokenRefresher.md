# Register a token refresher plugin

Registers a token refresher implementation

## Usage

``` r
registerTokenRefresher(name, refresher)
```

## Arguments

- name:

  Character string. Name of the refresher

- refresher:

  List with required methods: init, start, stop, isRunning, getToken

## Value

No meaningful value - called for its side effect of putting the
refresher into the registry under `name`. Stops with an error when
`refresher` is not a list, or when it does not carry all five of `init`,
`start`, `stop`, `isRunning` and `getToken`. Registering a name twice
replaces the earlier entry.
