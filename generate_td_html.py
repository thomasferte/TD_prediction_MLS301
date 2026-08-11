import nbformat
import subprocess
from pathlib import Path

NOTEBOOK = Path("td_prediction.ipynb")

TEACHER_DIR = Path("teacher")
STUDENT_DIR = Path("student")

TEACHER_HTML = "td_prediction_teacher.html"
STUDENT_NOTEBOOK = STUDENT_DIR / "td_prediction_student.ipynb"
STUDENT_HTML = "td_prediction_student.html"


def create_directories():
    TEACHER_DIR.mkdir(exist_ok=True)
    STUDENT_DIR.mkdir(exist_ok=True)


def create_student_notebook():
    """Remove cells tagged as solution."""
    nb = nbformat.read(NOTEBOOK, as_version=4)

    nb.cells = [
        cell
        for cell in nb.cells
        if "solution" not in cell.metadata.get("tags", [])
    ]

    nbformat.write(nb, STUDENT_NOTEBOOK)
    print(f"Created {STUDENT_NOTEBOOK}")


def convert_to_html(notebook, output_dir, output_name):
    """Convert notebook to HTML."""
    subprocess.run(
        [
            "jupyter",
            "nbconvert",
            str(notebook),
            "--to",
            "html",
            "--output-dir",
            str(output_dir),
            "--output",
            output_name,
        ],
        check=True,
    )

    print(f"Created {output_dir / output_name}")


if __name__ == "__main__":

    create_directories()

    # Teacher version
    convert_to_html(
        NOTEBOOK,
        TEACHER_DIR,
        TEACHER_HTML
    )

    # Student version
    create_student_notebook()

    convert_to_html(
        STUDENT_NOTEBOOK,
        STUDENT_DIR,
        STUDENT_HTML
    )

    print("Done!")