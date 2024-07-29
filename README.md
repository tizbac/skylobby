# skylobby [![license](https://img.shields.io/github/license/skynet-gh/skylobby)](LICENSE)

A Multiplayer Lobby Client that lets the user connect to various multiplayer servers which use [uberserver's](https://github.com/spring/uberserver) protocol and host games which use the [Spring RTS](https://springrts.com/) or [Recoil](https://beyond-all-reason.github.io/spring/) game engines. It'll let users register and connect to the various servers, join battle rooms and will automatically download the required game, map and engine packages. After setting up and downloading the requirements, users can play in the hosted rooms, set up their own or start local single player battles.

This is a fork from the original [Skylobby project](https://github.com/skynet-gh/skylobby/), starting from release 0.9.31. Releases will stick to the a.b.cXXX convention where the a.b.c prefix matches the latest release it's based off of from the original repository and the XXX is the local release number.
<sub>(done for compatibility with the [windows versioning constraints](https://stackoverflow.com/a/52414691) and the installer generation workflow)</sub>

This version adds the Metal Factions server to the default server list and has a few other UI improvements and changes to default settings to better fit the requirements of the Metal Factions community. It's still meant to be a generic lobby client, though.

## Usage

Install instructions and basic usage can be found in the [User Guide](https://github.com/skynet-gh/skylobby/wiki/User-Guide).

Feel free to open an [issue](https://github.com/springraaar/skylobby/issues) if you find a bug or have a feature request.

## Dev

Below are instructions on how to build and contribute to skylobby.

You will need [Java 11 or higher](https://adoptium.net/) and the [Clojure CLI tools](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools).

### REPL

Runs an interactive compiler as well as the UI. If you make a change to something in `src/clj` it will trigger a recompile and re-render the UI from the running state.

You will also need to install `rlwrap`. Run:

```bash
clj -M:nrepl
```

Logs are written to `repl.log`.

### Jar

To build an executable jar file:

```bash
clojure -M:uberjar
```

And then run it with:

```bash
java -jar target/skylobby.jar
```

To build an installer, then run `jpackage` for your platform, for example on Windows

```bash
jpackage @jpackage/common @jpackage/windows
```
