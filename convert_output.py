import ast
import logging
from argparse import ArgumentParser
from pathlib import Path

import pandas as pd


def parse_removals(removals_string):
    # Convert the string representation of the tuple into an actual list of tuples using ast.literal_eval
    removals_data = ast.literal_eval(removals_string)
    
    # Create a DataFrame from the list of tuples
    removals_df = pd.DataFrame(removals_data, columns=['no_removal', 'node_id', 'node_score', 'lcc.norm', 'slcc.norm'])
    return removals_df


def process_csv(file_path):
    # Read the CSV file
    df: pd.DataFrame = pd.read_csv(file_path)

    df.sort_values(
        by=["network",
            "heuristic",
            "r_auc",
            ],
        ascending=True,
        inplace=True,
    )
    df.drop_duplicates(
        subset=["network",
                "heuristic",

                ],
        keep="first",
        inplace=True,
    )
    # Initialize an empty DataFrame to store the expanded dataset
    expanded_df = pd.DataFrame()

    # Iterate over each row in the DataFrame
    for index, row in df.iterrows():
        # Parse the 'removals' column using the function defined above
        removals_df = parse_removals(row['removals'])

        # For each other column, repeat the value for the length of the removals_df
        for col in df.columns.difference(['removals']):
            removals_df[col] = row[col]

        # Append the result to the expanded DataFrame
        expanded_df = pd.concat([expanded_df, removals_df], ignore_index=True)

    return expanded_df

if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)

    logger = logging.getLogger(__name__)

    parser = ArgumentParser()
    parser.add_argument(
        "--file",
        type=Path,
        default=None,
        required=True,
        help="Path to the input CSV file.",
    )

    parser.add_argument(
        "-v",
        "--verbose",
        type=str.upper,
        choices=["INFO", "DEBUG", "WARNING", "ERROR"],
        default="info",
        help="Verbosity level (case insensitive)",
    )

    args = parser.parse_args()

    logger.setLevel(logging.getLevelName(args.verbose))

    if args.file is None:
        logger.error("Please provide a file path.")
        exit(1)

    if not args.file.exists():
        logger.error(f"File {args.file} does not exist.")

        exit(1)

    if not args.file.is_file():
        logger.error(f"Path {args.file} is not a file.")

        exit(1)

    # Convert library's output into a standard csv
    file_path = str(args.file)
    result_df = process_csv(file_path)

    logger.debug(result_df.head())

    output_file_path = file_path.replace('.csv', '_expanded.csv')

    if Path(output_file_path).exists():
        logger.warning(f"Output file {output_file_path} already exists. "
                       f"It will be overwritten."
                       )

    # Save the expanded DataFrame to a new CSV file
    result_df.to_csv(str(output_file_path), index=False)
    logger.info(f"Expanded output file saved to {output_file_path}")
