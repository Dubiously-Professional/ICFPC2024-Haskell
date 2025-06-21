# ICFPC2024-Haskell

Haskell implementation of [ICFPC 2024](https://icfpcontest2024.github.io/task.html) interpreter. To use this, you will need to run a local copy of the contest server following the instructions [here](https://github.com/icfpcontest2024/icfpc2024)

To build the project, run

```
stack build
```

then go get yourself a cup of coffee, work on some chores, maybe start on that novel you've been meaning to write. With any luck, by the time you are finished with all that, the project will be ready to run. I have implemented a simple terminal which you can run with

```
stack exec communicator
```

a good starting command is

```
get index
```

The communicator only supports decoding string expressions, which will allow you to access the lower-level projects. Higher-level projects will require more advanced expressions, which you can get from the translator:

```
stack exec translator
```

You can send commands with this the same way you can with `communicator`. It will then show you the response from the server translated into equivalent Haskell. Eventually, I would like to integrate compiling and running this into a single executable, but for now, you can copy-paste the result into a Haskell file or evaluate it in GHCI like so:

```
$ stack ghci
ghci> :l src/Translator.hs
ghci> <copy-paste the entire generated Haskell>
```
