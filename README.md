# Gary - Your Surprisingly Helpful Terminal Assistant

Gary is a Haskell-based AI assistant that runs in your terminal. It uses OpenAI's GPT-4.1 model to respond to your queries and can execute shell commands on your behalf. Think of Gary as your helpful intern who's surprisingly efficient and weirdly fast.

## Features

- Interactive terminal-based chat interface
- Executes shell commands on request
- Maintains conversation history
- Provides helpful responses using OpenAI's GPT-4.1 model

## Prerequisites

- [Haskell](https://www.haskell.org/ghcup/) (GHC 9.4 or later recommended)
- [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) build tool
- OpenAI API key

## Installation

1. Clone this repository:
   ```
   git clone https://github.com/yourusername/gary.git
   cd gary
   ```

2. Build the project using Stack:
   ```
   stack build
   ```

## Configuration

Before running Gary, you need to set your OpenAI API key as an environment variable:

```bash
export OPENAI_KEY=your_openai_api_key_here
```

You can add this to your shell profile file (`.bashrc`, `.zshrc`, etc.) to make it persistent.

## Usage

Run Gary using Stack:

```bash
stack run
```

This will start an interactive session where you can chat with Gary. The prompt will show your current directory:

```
🛠️ gary>
```

Type your questions or commands and press Enter. Gary will respond and can execute shell commands when asked.

### Examples

Ask Gary for information:
```
🛠️ gary> What's the current time?
```

Ask Gary to run a command:
```
🛠️ gary> List all files in the current directory
```

Ask Gary to help with a task:
```
🛠️ gary> Create a simple "Hello World" Haskell program
```

## Project Structure

- `gary/`: Main executable package
  - `app/Main.hs`: Core application logic
- `format/`: Text formatting library used by Gary
  - `src/Text/`: Text formatting utilities

## Building with Stack

This project uses Stack for building and dependency management. Some useful Stack commands:

- `stack build`: Build the project
- `stack run`: Run the Gary executable
- `stack ghci`: Start a GHCI session with the project loaded
- `stack test`: Run the test suite (if tests are added)

## License

This project is licensed under the Apache 2.0 License - see the LICENSE file for details.
