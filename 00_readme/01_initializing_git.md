# Initializing Git

This file gives a quick set of instructions for setting up the Git repository.

## Instructions

1. Open Git Bash in the root directory for this project.
2. Run the following commands to remove references to repository for template:
   1. `rm -rf .git`
   2. `git init`
3. Go to github.com
   1. Create your new repository.
4. From Git Bash run the command directed by github.com:
   1. `git remote add origin https://github.com/Dee-L/<repository name goes here>.git`
5. Use either VS Code shortcuts or GitBash commands according to the table below to finish initializing the repository:

| Step                    | VS Code key binding | GitBash command                   |
| ----------------------- | ------------------- | --------------------------------- |
| Add files/stage changes | `ctrl + g a`        | `git add .`                       |
| Commit changes          | `ctrl + g c`        | `git commit -am '<message here>'` |
| Push changes to remote  | `ctrl + g p`        | `git push -u master`              |
