# AI Othello

https://github.com/if-h4102/ai-othello

**Note for people reading this outside of GitHub**: this is a MarkDown file.
You can read it as raw text, but it's more convenient to use some browser plugin
or simply read it directly on GitHub (see the previous link).

## Overview

This app allows two players to play an [Othello game](https://en.wikipedia.org/wiki/Reversi),
also known as _Reversi_.

The two players can be human or one of the developped AIs:

0. A totally random AI.
1. Another random AI.
2. A deterministic AI.
3. An AI using the _min-max_ algorithm.

**Note that the more the AI's number is high, the more it's supposed to be efficient.**

You'll find how to play the game in the following section.

## Usages

### 1. Play using the console
  
    > swipl src/main.pl
    
Then follow instructions to setup players (-1 for human player, [0, .., 3] for AI) and play.

Once the gave finished you can start an other one by calling on the prolog interpreter:

    > ?- main:run.

**Example:** (Human versus _min-max_ AI)

    What is the type of the first player which will use the symbol x ?
    Type -1 for a human player or a number between 0 and 3 to select an ai with the given level.
    |: -1.
    What is the type of the second player which will use the symbol o ?
    Type -1 for a human player or a number between 0 and 3 to select an ai with the given level.
    |: 3.
    
        1   2   3   4   5   6   7   8
      ---------------------------------
    1 |   |   |   |   |   |   |   |   |
      ---------------------------------
    2 |   |   |   |   |   |   |   |   |
      ---------------------------------
    3 |   |   |   |   |   |   |   |   |
      ---------------------------------
    4 |   |   |   | x | o |   |   |   |
      ---------------------------------
    5 |   |   |   | o | x |   |   |   |
      ---------------------------------
    6 |   |   |   |   |   |   |   |   |
      ---------------------------------
    7 |   |   |   |   |   |   |   |   |
      ---------------------------------
    8 |   |   |   |   |   |   |   |   |
      ---------------------------------
     number of tokens -  o : 2 | X : 2
    You use the symbol : x
    At which abscissa do you want to play ?
    |: 3.
    At which ordinate do you want to play ?
    |: 5.
        1   2   3   4   5   6   7   8
      ---------------------------------
    1 |   |   |   |   |   |   |   |   |
      ---------------------------------
    2 |   |   |   |   |   |   |   |   |
      ---------------------------------
    3 |   |   |   |   |   |   |   |   |
      ---------------------------------
    4 |   |   |   | x | o |   |   |   |
      ---------------------------------
    5 |   |   | x | x | x |   |   |   |
      ---------------------------------
    6 |   |   |   |   |   |   |   |   |
      ---------------------------------
    7 |   |   |   |   |   |   |   |   |
      ---------------------------------
    8 |   |   |   |   |   |   |   |   |
      ---------------------------------
     number of tokens -  o : 1 | X : 4
        1   2   3   4   5   6   7   8
      ---------------------------------
    1 |   |   |   |   |   |   |   |   |
      ---------------------------------
    2 |   |   |   |   |   |   |   |   |
      ---------------------------------
    3 |   |   |   |   |   |   |   |   |
      ---------------------------------
    4 |   |   |   | x | o |   |   |   |
      ---------------------------------
    5 |   |   | x | o | x |   |   |   |
      ---------------------------------
    6 |   |   | o |   |   |   |   |   |
      ---------------------------------
    7 |   |   |   |   |   |   |   |   |
      ---------------------------------
    8 |   |   |   |   |   |   |   |   |
      ---------------------------------
     number of tokens -  o : 3 | X : 3
    You use the symbol : x
    At which abscissa do you want to play ?
    |:

And so on until the end of the game.

### 2. Play using a graphical interface

We developped a graphical interface separatly (you'll find it [here](https://github.com/Sn0wFox/ai-othello-web-gui)).
To do so, we created a prolog server using a REST API.

Here are the developped endpoints:

* GET   /api/board/initial
* POST  /api/board/update
* POST  /api/play
* POST  /api/play/validate
* POST  /api/play/able

They are very well documented on the file `src/server.pl`.

### Launch the server

Just run

    swipl src\server.pl
    
### Launch the graphical interface

First please install [Node.js](https://nodejs.org/en/)

    git clone https://github.com/Sn0wFox/ai-othello-web-gui.git
    cd ai-othello-web-gui
    npm install -g gulp typescript typings
    npm install
    typings install
    gulp build
    node build\server.js
    
### Go to the provided URL in your favorite browser

Usually `http://localhost:3000/`.

Then enjoy.

### Overview

Here's what it looks like:

![ihm](https://cloud.githubusercontent.com/assets/15182422/21049118/3e2f8936-be13-11e6-9d84-a6a36e09e35c.gif)
    
### 3. Performances

You'll need to have [pyhton](https://www.python.org/) installed.

    > py src\profile.py
    
Please note that advanced AIs are slower, and so performances tests

### 4. Tests

To run all the tests, just run

    swipl test/main.pl

### 5. Debug mode

    > swipl src/main.pl
    
From here you can access everything. (you will first have to play a game)
    
## Requierements - SWIprolog

### Install

#### Ubuntu

    sudo apt-get install swiprolog

can be used with the command: 'prolog'

for more information please read 'man'

#### Windows

Download the msi-installer you need [here](http://www.swi-prolog.org/download/stable).

Be sure to update your `PATH` environment variable to include the path to the executable file `swipl-win.exe`.

### Usage

Just run

    swipl <whatever_you_need>
  
For more information, please take a look at [this page](http://www.swi-prolog.org/pldoc/man?section=cmdline).
