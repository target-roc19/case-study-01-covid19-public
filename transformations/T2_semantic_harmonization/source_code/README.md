# Using git subtree

`git subtree` allows you to copy one Git repository inside another as a subdirectory. This allows you to manage shared functionality across several independent repositories while using normal Git language.

This repository is used as a git subtree in all the case studies to source code for the transformation steps. **Add as a git subtree with `git subtree add --prefix transformations/T2_semantic_harmonization/source_code https://github.com/target-roc19/UMCU-T2 main --squash`**

### Terminology

**Parent repositories-** The case study repositories that consume shared functionality.

**Child repository-** Contains shared logic used by all Parent repos. A single Child repo may be included in many Parent repos.
  Example: `https://github.com/target-roc19/process-persons-observations`

### 1. Adding a subtree

To insert the contents of the child repository into a directory in the parent repository:

```
git subtree add --prefix path/to/parent/directory https://github.com/url/to/child main --squash
```

This:

* Copies the main branch of the child repo into the parent directory
* Squashes its entire history (from the main branch) into one commit
* Records the commit id of the subproject that has been injected. Using this information the subsequent git subtree pull can retrieve the previous integration point as base for the next squash/merge. (ii)
* A squashed subtree add produces two commits: (i) Merge commit joining subtree history, and (ii) Squashed commit containing subtree content

### 2. Updating the subtree

To update the imported subtree at the parent to the latest state of the child:

```
git subtree pull --prefix path/to/parent/directory https://github.com/url/to/child main --squash
```

You may optionally pull only from tags like `v1.0` instead of `main`.

### 3. Pushing changes to the subtree

If you modify the subtree inside the Parent repo:

```
git subtree push --prefix path/to/parent/directory https://github.com/url/to/child new_branch
```

This creates commits in the Child repo containing only the subtree changes.

### 4. Time-Saving Git Aliases

If you prefer, add to `~/.gitconfig`:

```
[alias]
sba = "!f() { git subtree add --prefix=$2 $1 master --squash; }; f"
sbu = "!f() { git subtree pull --prefix=$2 $1 master --squash; }; f"
```

Usage:

```
git sba `<repo-url>` `<folder>`
git sbu `<repo-url>` `<folder>`
```

### References

1. [Issue 21: Why git subtree over git submodules](https://github.com/target-roc19/target-core-umcu/issues/21)
2. [Atlassian article](https://www.atlassian.com/blog/developer/the-power-of-git-subtree)
