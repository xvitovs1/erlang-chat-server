# erlang-chat-server

School project for course MTAT.08.022 Concurrent programming languages at University of Tartu.

**Task:** Implement chat server in Erlang.

## How to run the chat server and the client

To run the server, just compile server.erl and server_supervisor.erl and run server_supervisor:start() method.

Client is implemented in Haskell using gtk 3. You probably need to install [Haskell stack](http://docs.haskellstack.org/en/stable/install_and_upgrade/) and then run it like this (on Linux):

In client folder, run `stack build client`. (If this does not work, you probably need to do `stack setup` first.)

**Dependencies:**
* libglib2.0-dev
* libcairo2-dev
* libpango1.0-dev
* libgtk-3-dev

After the client is built, run `stack exec client`.

## Description

**Chat server** is implemented using parallel server, so when it accepts a new connection, it creates a new process for it.

First server waits for user to pick a username -- waits for message in format `connect:username`. Checks if username is available and if it is, it goes to main loop where it receives other messages.

Another 3 types of messages can be sent to server:
* `bcast:message` - broadcasts message
* `pm:to:message` - send private message to a given user
* `disconnect` - disconnects user from server

Chat server sends `error:message` and `info:message` messages to the client.

*Server_supervisor* is a supervisor for the chat server. If the server could not be established (socket was probably already in use), it tries to restart in 10 seconds.

**Client:**

First, user has to type a username. If username is not already taken, new screen appears. 

If you want to send a private message, fill in the field 'To' and message and send.

If you want to broadcast a message, do not fill the field 'To', fill in only the message and send.

When client is closed, user is disconnected.

