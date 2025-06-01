import re
import os

def generate_markdown_filename(f90_filepath):
    """Generates the output Markdown filename."""
    filename = os.path.basename(f90_filepath)
    # Replace .f90 with _f90.md
    md_filename = filename.replace(".f90", "_f90.md")
    # Add src_ prefix
    md_filename = "src_" + md_filename
    return os.path.join("docs", "src_docs", md_filename)

def parse_fortran_file(content):
    """Parses Fortran file content to extract components."""
    modules = []
    subroutines = []
    functions = []
    used_modules = []
    external_libs_found = set() # Use a set to store unique library prefixes

    # Regex patterns
    module_pattern = re.compile(r"^\s*MODULE\s+([a-zA-Z_][a-zA-Z0-9_]*)", re.IGNORECASE)
    subroutine_pattern = re.compile(r"^\s*SUBROUTINE\s+([a-zA-Z_][a-zA-Z0-9_]*\s*\(.*?\))", re.IGNORECASE)
    # Simpler function pattern, might need refinement for RESULT clause or TYPE prefix
    function_pattern = re.compile(r"^\s*(?:TYPE\s*\(.*?\)\s*FUNCTION|FUNCTION)\s+([a-zA-Z_][a-zA-Z0-9_]*\s*\(.*?\))", re.IGNORECASE)
    use_pattern = re.compile(r"^\s*USE\s+([a-zA-Z_][a-zA-Z0-9_]*)", re.IGNORECASE) # Simplified, captures module name

    # External library prefixes
    lib_prefixes = ["MPI_", "omp_", "libxc_", "fftw_"]

    for line in content.splitlines():
        line_stripped = line.strip()

        match = module_pattern.match(line_stripped)
        if match:
            modules.append(match.group(1))
            continue # A line is either a module, sub, or func declaration

        match = subroutine_pattern.match(line_stripped)
        if match:
            subroutines.append(match.group(1))
            continue

        match = function_pattern.match(line_stripped)
        if match:
            functions.append(match.group(1))
            continue

        match = use_pattern.match(line_stripped)
        if match:
            used_modules.append(match.group(1))

        for prefix in lib_prefixes:
            if prefix.lower() in line_stripped.lower(): # case-insensitive check for library calls
                # Attempt to extract a meaningful name if possible, otherwise use prefix
                # This is a simple heuristic
                lib_name_match = re.search(r"("+ re.escape(prefix) + r"[a-zA-Z0-9_]+)", line_stripped, re.IGNORECASE)
                if lib_name_match:
                    external_libs_found.add(lib_name_match.group(1))
                else:
                    external_libs_found.add(prefix.strip('_'))


    return {
        "modules": modules,
        "subroutines": subroutines,
        "functions": functions,
        "used_modules": list(set(used_modules)), # Remove duplicates
        "external_libs": sorted(list(external_libs_found))
    }

def main():
    try:
        with open("f90_files.txt", "r") as f:
            f90_files = [line.strip() for line in f if line.strip()]
    except FileNotFoundError:
        print("Error: f90_files.txt not found.")
        return
    except Exception as e:
        print(f"Error reading f90_files.txt: {e}")
        return

    try:
        with open("fortran_doc_template.md", "r") as f:
            template_content = f.read()
    except FileNotFoundError:
        print("Error: fortran_doc_template.md not found.")
        return
    except Exception as e:
        print(f"Error reading fortran_doc_template.md: {e}")
        return

    os.makedirs(os.path.join("docs", "src_docs"), exist_ok=True)

    for f90_filepath in f90_files:
        output_md_filepath = generate_markdown_filename(f90_filepath)
        markdown_content = template_content # Start with a fresh template copy

        # Replace overview placeholder
        markdown_content = markdown_content.replace(
            "[Brief description of the file's purpose and role]",
            "[Please provide a brief description of this file's purpose and role.]"
        )
        # Replace important variables placeholder
        markdown_content = markdown_content.replace(
            "[List of important variable/constant names, their types (if determinable), and descriptions of their purpose, especially for parameters or widely used variables. E.g.:...]",
            "[Please list and describe important variables and constants, their types, and their purpose.]"
        )
        # Placeholder for called subroutines/functions
        markdown_content = markdown_content.replace(
             "[List other subroutines/functions within the project that this file calls. If extensive, summarize key interactions.]",
             "[Please list other subroutines/functions within the project that this file calls.]"
        )


        file_content = ""
        try:
            with open(f90_filepath, "r") as f:
                file_content = f.read()
        except FileNotFoundError:
            print(f"Warning: Source file {f90_filepath} not found. Creating minimal doc.")
        except Exception as e:
            print(f"Warning: Could not read {f90_filepath}: {e}. Creating minimal doc.")

        if not file_content:
            # Use placeholders for all components if file is empty or unreadable
            components_section = (
                "### Modules\n\n[Could not automatically parse components or no components found. Please fill this section manually.]\n\n"
                "### Subroutines\n\n[Could not automatically parse components or no components found. Please fill this section manually.]\n\n"
                "### Functions\n\n[Could not automatically parse components or no components found. Please fill this section manually.]"
            )
            dependencies_section = (
                "#### Used Modules\n\n[Could not automatically parse components or no components found. Please fill this section manually.]\n\n"
                "#### External Library Dependencies\n\n[Could not automatically parse components or no components found. Please fill this section manually.]"
            )
            markdown_content = markdown_content.replace("<!-- Key Components Placeholder -->", components_section)
            markdown_content = markdown_content.replace("<!-- Dependencies Placeholder -->", dependencies_section)

        else:
            parsed_data = parse_fortran_file(file_content)

            # Populate Key Components
            modules_md = "\n".join(f"- `{name}`" for name in parsed_data["modules"]) if parsed_data["modules"] else "[Could not automatically parse components or no components found. Please fill this section manually.]"
            subroutines_md = "\n".join(f"- `{sig}`" for sig in parsed_data["subroutines"]) if parsed_data["subroutines"] else "[Could not automatically parse components or no components found. Please fill this section manually.]"
            functions_md = "\n".join(f"- `{sig}`" for sig in parsed_data["functions"]) if parsed_data["functions"] else "[Could not automatically parse components or no components found. Please fill this section manually.]"

            components_section = f"### Modules\n\n{modules_md}\n\n### Subroutines\n\n{subroutines_md}\n\n### Functions\n\n{functions_md}"
            markdown_content = markdown_content.replace("<!-- Key Components Placeholder -->", components_section)

            # Populate Dependencies
            used_modules_md = "\n".join(f"- `{name}`" for name in parsed_data["used_modules"]) if parsed_data["used_modules"] else "[Could not automatically parse components or no components found. Please fill this section manually.]"
            external_libs_md = "\n".join(f"- `{lib}`" for lib in parsed_data["external_libs"]) if parsed_data["external_libs"] else "[No common external library calls detected or parsing failed. Please review manually.]"

            dependencies_section = (
                f"#### Used Modules\n\n{used_modules_md}\n\n"
                f"#### External Library Dependencies\n\n{external_libs_md}"
            )
            markdown_content = markdown_content.replace("<!-- Dependencies Placeholder -->", dependencies_section)
            markdown_content = markdown_content.replace("[List other subroutines/functions within the project that this file calls. If extensive, summarize key interactions.]", "[Please list other subroutines/functions within the project that this file calls.]")


        try:
            with open(output_md_filepath, "w") as f:
                f.write(markdown_content)
            print(f"Successfully generated: {output_md_filepath}")
        except Exception as e:
            print(f"Error writing Markdown file {output_md_filepath}: {e}")

if __name__ == "__main__":
    main()
