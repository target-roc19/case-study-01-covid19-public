# Repository Template for TARGET
This repository serves as a template for creating new project repositories within the `target-roc19` GitHub organisation. It includes the basic structure and configuration files designed to streamline project setup and maintain consistency across repositories.

## Why Use This Template?
- Standardised folder structure for all projects
- Pre-configured `.gitignore`, license, and README files
- Consistent coding and documentation guidelines
- Simplifies repository setup for new projects

## Why This Structure?
- Follows established coding practices: The folder layout and included files are designed to promote clean, maintainable, and reproducible code across all projects within the organisation. 
- Encourages consistency: By standardising the structure, it’s easier for team members to navigate and understand any repository, reducing onboarding time and miscommunication.
- Supports collaboration: Clear separation of data, scripts, tests, and documentation facilitates parallel work and code review processes.

## Contributing
- Please open an issue before starting a major change.
- Use feature branches and submit pull requests for review.
- Follow coding guidelines, see [coding-practices](https://github.com/target-roc19/coding-practices)

## How to Use This Template
1. Go to this repository on GitHub.
2. Click the “Use this template” button.
3. Choose a name for your new repository (e.g., `case-study-xx-description`) and configure the visibility settings.
4. Click Create repository from template.
5. Replace the placeholder content with study-specific details. 
6. Clone your new repository and start working!

## Template Contents
- `.github/workflows` – Automation for tests, style checks, and CI/CD tasks
- `/data` – Folder for datasets
- `/docs` – Documentation files
- `/logs` – Log files generated during pipeline execution
- `/run` - Scripts for pipeline execution and orchestration
- `/tests/testhat` – Testing framework
- `/transformations` – Code and analysis scripts for data processing and transformations
- `.gitignore` – Pre-configured to exclude common files tailored to R
- `CHANGELOG.md` - Records notable changes to the project over time, following a consistent format
- `LICENSE` – Licensing information
- `README.md` – Project overview and usage instructions

## Customising Your New Repository
After creating a new repo from this template:
- Update the README to reflect your project specifics.
- Adjust folder structure if needed to fit your project needs.
- Set up continuous integration or other automation tools as necessary.

## Support & Questions
For questions or help regarding this template or creating new repositories, please contact:
- Linda Nab [[l.nab@umcutrecht.nl](lnab@umcutrecht.nl)]
- Mattia Cinelli [[m.cinelli@umcutrecht.nl](m.cinelli@umcutrecht.nl)]

## License
XXX