## GHLint

A github action to review pull requests with hlint leaving a comment, 
to use it in your repo add the following to your workflow (after checking out the repository).


```yaml
- name: Check code
   uses: aktowns/ghlint@master
   with:
     token: ${{ secrets.GITHUB_TOKEN }}
     path: ./
```
