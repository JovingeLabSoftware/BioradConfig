# RedGuru -- Linking RedCap and LabGuru

## Background

This package will provide the middleware for linking REDCap and LabGuru. If all goes as planned, data syncs will be triggered by a REDCap DET. DETs send a `POST` request to a specified server that includes which record was updated and what project it was from. DETs are currently not enabled for Spectrum REDCap projects, but a request has been sent asking IT to enable.


---------------------------------


## Security

I think the best way to handle this is to have a API exposed on our lab server running on a port that only accepts incoming HTTP requests from the REDCap server. This should handle any security issues b/c nothing else will be able to connect to it.


---------------------------------


## Implementation

Still deciding on the best way to do this. Could try out the new [plumber](http://plumber.trestletech.com/) package in `R`, but that is in the early stages of dev and is not ready for prime time. Building a simple API with Python/Flask would be a good solution. Took the time to follow a tutorial on how to do this and stored in `~/projects/todo-api/`.

That API could handle the incoming requests and launch the necessary API call back to REDCap via the command line or to a running `Rserve` instance via `pyRserve`. The second option might be the way to go if we are getting a ton of requests.

And does my API need to be async? http://tavendo.com/blog/post/going-asynchronous-from-flask-to-twisted-klein/
