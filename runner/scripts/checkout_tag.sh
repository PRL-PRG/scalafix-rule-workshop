git fecth --tags
latestTag=$( git describe --tags `git rev-list --tags --max-count=1` )
git checkout $latestTag
