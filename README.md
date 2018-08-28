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

Install stack and...

```
stack setup
stack build
```

Or use Docker:
`docker build -t lastfm-dump .`

## Usage

To sync your data to Mongo, start your mongod and run `lastfm-dump` with the following env vars set either in `.env` file or environmental variables: 

```
MONGODB_PASSWORD=<secret>
MONGODB_USER=<secret>
MONGODB_DBNAME=<secret>
MONGODB_ADDRESS=<secret>
MONGODB_PORT=<secret>
LASTFM_API_KEY=<secret>
PAGE_SIZE=200
LASTFM_USER=myUser
```
