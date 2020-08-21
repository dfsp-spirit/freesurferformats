## Contributing to freesurferformats

I am very happy to accept [pull requests](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request), provided you are fine with publishing your work under the [license of this project](https://github.com/dfsp-spirit/freesurferformats#license). If your PR is not just a fix but changes or adds lots of code, please get in touch by [opening an issue](https://github.com/dfsp-spirit/freesurferformats/issues) before starting the project so we can discuss it first. Development currently happends on the develop branch.

If you want to contribute something, the general workflow is:

- Log into the Github website and fork the freesurferformats repository to your account.
- Checkout your forked repository to your computer. You will be on the master branch. Change to the develop branch.
- Create a new branch and name it after your feature, e.g., `add_cool_new_feature` or `fix_issue_17`.
- Make changes to the freesurferformats code and commit them into your branch.
- Make sure the existing unit tests are all green (see below). Adding new tests for your fix or the new features is even better.
- Create a pull request, requesting to merge your branch into the develop branch of my freesurferformats repo.

## Setting up the development environment

Most likely you already have your development environment setup the way you prefer it when you decide to contribute. If not, here is a quick way to get started.

Note that you do not have to use rstudio of follow these suggestions, any editor or IDE will do.

- Make sure you have R installed
- Download and install the latest version of rstudio (I use the free RStudio Desktop Open Source Edition)
- In your shell, change into your checkout of your fork of freesurferformats (see above)
- run 'rstudio freesurferformats.Rproj'
- install the required development packages listed on the freesurferformats website
- In rstudio, make sure all required development packages are installed and you are ready to go:
  * Build the package including the documentation (Menu > Build > Build Source Package) and install/load it (Menu > Build > Clean and Rebuild).
  * Run the unit tests (Menu > Build > Check Package).
  * Generate the test coverage report (Item bar > Addins > Report test coverage for a package). Wait until the report shows up in the Viewer.
