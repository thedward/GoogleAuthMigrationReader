## Usage

The executable `google-auth-read-export` accepts the text representation of a Google Authenticator export (a URI beginning with `otpauth-migration://`) and outputs a list of `otpauth://` URIs. 

```.sh
google-auth-read-export 'otpauth-migration://offline?data=...'
```

## Building

I've only tested the build using `cabal build` and GHC 8.10.7
