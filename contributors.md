
We welcome contributions to the project. 
To submit a contribution, please follow [the usual fork/pull request workflow](https://gist.github.com/Chaser324/ce0505fbed06b947d962). 

Even if your patch is a small bug fix, you might want to discuss it in an issue first to make sure that nobody is already working on it.

## A note about hooks

Please create the necessary git hooks to run before checking code in:

 ```
 > cd root/of/repo
 # Note the two levels of indirection.
 # This is because git takes .git/hooks as the cwd when executing hooks.
 > ln -s ../../scripts/build/hooks/pre-commit.sh .git/hooks/pre-commit
 ```

For the scalafmt git hook, the script in `scripts/build/format-all.sh` can be used to clean up the project before committing.




