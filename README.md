# realm - Reason + React + Elm

## Design goals
* React interoperability
* Implicit message dispatch on events, i.e. (`onClick(Increment)` instead of `onClick(_event => dispatch(Increment))`)
* Extendable with custom props and prop handlers
* Composable message/model types