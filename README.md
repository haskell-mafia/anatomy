anatomy
=======

```
Anatomy: a study of the structure or internal workings of something.
```

This is the central hub for collecting, describing, classifying and initialising projects.


Creating a new project
----------

TODO: Elaborate

Open a pull request for your topic branch. Once it is has been approved and merged to master,
then Anatomy will automatically create the github project for you, the jenkins jobs (for master and PR branches), and setup all the appropriate webhooks for hipchat, Jenkins etc (so you are ready to go).

Also, if there exists an appropriate template for your project type, then your empty repo will be pre-populated with the template contents (more templates are always welcome).

#### But I already have a project in github setup!

Well you can still go through the above, but you will have to setup your github project, webhooks/jobs etc by hand, that is fine. Sync won't change anything for you, but it is nice to have your project defined in Anatomy anyway.


Operations
----------

### diagnose / sync

For each project check current state, has repo, builds etc... go and add what is required for sync.
Sync currently runs automatically from master branch when pushed (via the ci server) as it has permissions to see all github repos.
Users running sync may not have permissions to see all repos so leave it to the CI server to run.
Sync will not touch any currently existing projects.

### stats / reports

(WIP)

Pretty pictures etc... to help us visualize projects.

Statistics scraped from github. PRs merged per week, avg. durations etc...
