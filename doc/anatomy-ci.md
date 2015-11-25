anatomy-ci
==========

A tool to help configure infrastructure and tooling for our projects.

## Usage

### Authentication

Generate an oauth token on github:

 - <https://github.com/settings/applications>
 - Personal Access Token > Generate New Token
 - Permissions: read/write/admin hooks, repos, public repos


### Configuration

```
export GITHUB_OAUTH={TOKEN}
export HIPCHAT_TOKEN={TOKEN}
export HIPCHAT_ROOM=builds
export CI_COMMAND="./bin/ci"
```

#### Optional Configuration


```
export JENKINS_HOST=http://myOtherJenkins.localhost
```

### Generate Hooks for a specific project

```
#                       anatomy-ci hook <github-org/account> <project>
./dist/build/anatomy-ci/anatomy-ci hook ambiata promulgate
```

### Generate Hooks for all ambiata projects.

```
#                       anatomy-ci hooks <github-org>
./dist/build/anatomy-ci/anatomy-ci hooks ambiata
```


### Extract config for a job on jenkins.

```
#                       anatomy-ci get-job <your-github-account> <jenkins-project>
./dist/build/anatomy-ci/anatomy-ci get-job markhibberd project > job.xml
```

### Create job on jenkins from template.

The template will replace `${project}` and any environment variable (prefixed with `env_`).

```
#                       anatomy-ci create-job <your-github-account> <jenkins-project> <github-repo> <template>
./dist/build/anatomy-ci/anatomy-ci create-job markhibberd mundane mundane templates/job.xml
```

### Update job on jenkins from template.

The template will replace `${project}` and any environment variable (prefixed with `env_`).

```
#                       anatomy-ci update-job <your-github-account> <jenkins-project> <github-repo> <template>
./dist/build/anatomy-ci/anatomy-ci update-job markhibberd mundane mundane templates/job.xml
```

### Generate a job from template, but only return it to stdout

```
#                       anatomy-ci generate-job <your-github-account> <jenkins-project> <github-repo> <template>
./dist/build/anatomy-ci/anatomy-ci generate-job markhibberd mundane mundane templates/job.xml
```


### Compare generated templates with what's in Jenkins

```
# scripts/compare-template.sh <your-github-account> <jenkins-project> <github-repo> <template>
scripts/compare-template.sh  markhibberd mundane mundane templates/job.xml
```

Note: requires `HIPCHAT_TOKEN`. Defaults a `HIPCHAT_ROOM` and `CI_COMMAND` value.

This uses the `generate-job` function to only generate a substituted XML file from the inputs, and diff against the jenkins job of the same name in the name assumed to live in `../jenkins-config/jobs/<JENKINS-PROJECT>/config.xml`

=======

## Template Variables

XML Templates in this repo can have variables. But Jenkins scripts can also have variables. Here's how to format each

`${env_HIPCHAT_ROOM}`
  - single-dollar variables must have an environment variable of the same name (in this case, `HIPCHAT_ROOM`) for the `substituteA` process to sub in values.

`$${JOB_NUMBER}`
  - double-dollar variables resolve into single-dollar variables for Jenkins
