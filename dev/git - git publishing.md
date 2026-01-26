# Initializing git and publishing as HTML
## Git init
```bash
git init
git add .
git commit -m "Initial commit"
git remote add origin <your-repo-url>
# For new ones
git push -u origin main 
# For existing ones
git branch -u origin/main # or git branch --set-upstream-to=origin/main

```
NOTE:
`git fetch` only updates the local copy of the remote; useful to see the differences before merging.

`git pull` is equivalent to `git fetch && git merge origin/main`
`git pull --rebase` is equivalent to `git fetch && git rebase origin/main`

## Quarto publish
```bash
quarto publish gh-pages
```

## On Github
On GitHub: Repo → Settings → Pages

Set Source to Deploy from a branch

Choose Branch = gh-pages and folder / (root)

Your site will be at:

https://<username>.github.io/<repo>/
