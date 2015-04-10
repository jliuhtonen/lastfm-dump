# lastfm-dump

Lastfm-dump is a tool that fetches your listening data from Last.fm and inserts them to Mongo DB so you can make all sorts of interesting stuff with your listening data.

The resulting BSON objects will look like this:

```
{
    "_id": ObjectId("5522c37ca7956d6b02000010"),
    "name": "Carrie & Lowell",
    "artist": {
        "name": "Sufjan Stevens",
        "mbid": "01d3c51b-9b98-418a-8d8e-37f6fab59d8c"
    },
    "album": {
        "name": "Carrie & Lowell",
        "mbid": "87b4a614-d53d-4495-b176-5d4f2bb353e6"
    },
    "url": "http://www.last.fm/music/Sufjan+Stevens/_/Carrie+&+Lowell",
    "scrobbledAt": ISODate("2015-04-05T20:56:41Z")
}
```

## Compiling

Install the latest version of Haskell Platform, and...

```
cabal sandbox init
cabal install --only-dependencies
cabal build
```

And you're all set.

## Usage

The program reads its configuration from config.json. You can find a sample configuration in file config.sample.json.

Then, to transfer your data to Mongo, start your mongod and run the dump with your username as the command line parameter: 

`lastfm-dump username`
