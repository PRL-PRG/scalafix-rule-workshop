# Tools for examining the use of implicits in large Scala codebases

## Installation:

### Unix (Debian, Ubuntu)

    1. Install dependency managers:

      `> sudo apt-get install python python-pip mysql-server`

    2. Install python dependencies:

      `> sudo pip install fabric termcolor`

    3. Donwload required tools:

      `> cd path/to/runner/ && fab setup`

### For contributors

Please create the necessary git hooks to run before checking code in:

 ```
 > cd root/of/repo
 # Note the two levels of indirection.
 # This is because git takes .git/hooks as the cwd when executing hooks.
 > ln -s ../../scripts/build/hooks/pre-commit.sh .git/hooks/pre-commit
 ```

For the scalafmt git hook, the script in `scripts/build/format-all.sh` can be used to clean up the project before commiting.




