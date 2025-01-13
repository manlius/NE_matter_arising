import ast

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


# Convert library's output into a standard csv
file_path = 'heuristics_entanglement_metadata.csv'
result_df = process_csv(file_path)
print(result_df.head())

result_df.to_csv(file_path.replace(".csv", "_expanded.csv"), index=False)

#
# # Convert library's output into a standard csv
# file_path = 'heuristics_entanglement_metadata_lcc.csv'
# result_df = process_csv(file_path)
# print(result_df.head())
#
# result_df.to_csv( file_path.replace(".csv", "_expanded.csv") , index=False)
