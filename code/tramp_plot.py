#!/Users/troyhinckley/blog/code/code-env/bin/python

import seaborn as sns
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
sns.set()

# Data for out of band copying
out_of_band_data = {
    0: 0.7577721009369377,
    # 1024: 0.7500566,
    # 2048: 0.7502688,
    # 4096: 0.7442152,
    8192: 0.7529396,
    16384: 0.7818844,
    32768: 0.817977,
    65536: 0.8587491999999999,
    131072: 0.8795438000000001,
    262144: 1.0668112,
    524288: 1.2524406000000001,
    1048576: 1.5225258,
    2097152: 1.8468724,
    4194304: 2.7893574,
    8388608: 3.6411000000000002
}

# Data for inline with compression
inline_compression_data = {
    0: 0.3958001492704499,
    # 1024: 0.44455539999999993,
    # 2048: 0.44564079999999995,
    # 4096: 0.5041148,
    8192: 0.5066934,
    16384: 0.5095282,
    32768: 0.5781666000000001,
    65536: 0.6150042,
    131072: 0.6621734,
    262144: 0.7339042,
    524288: 0.8488517999999999,
    1048576: 1.2970814000000002,
    2097152: 1.749291,
    4194304: 3.8350307999999997,
    8388608: 8.1427978
}

# Prepare data for Pandas DataFrame
df_out_of_band = pd.DataFrame(list(out_of_band_data.items()), columns=['File Size (Bytes)', 'Time (s)'])
df_out_of_band['Type'] = 'Out of Band'

df_inline_compression = pd.DataFrame(list(inline_compression_data.items()), columns=['File Size (Bytes)', 'Time (s)'])
df_inline_compression['Type'] = 'Inline Compression'

# Combine dataframes
df_combined = pd.concat([df_out_of_band, df_inline_compression])

# Create the plot
plt.figure(figsize=(10, 6))

# Plotting
plot = sns.lineplot(data=df_combined, x='File Size (Bytes)', y='Time (s)', hue='Type', 
                    palette={'Out of Band': 'blue', 'Inline Compression': 'green'}, errorbar=None, marker='o')

# Set labels
plt.xlabel("File Size")
plt.ylabel("Time (s)")
plt.title("File Transfer Time vs File Size")

# Set x-axis to log scale (base 2)
plot.set_xscale('log', base=2)

# Format x-axis to show KB/MB
def format_func(value, tick_number):
    if value >= 1024 * 1024:
        return f"{value / (1024 * 1024):.0f} MB"
    elif value >= 1024:
        return f"{value / 1024:.0f} KB"
    return f"{value:.0f} B"

plot.xaxis.set_major_formatter(plt.FuncFormatter(format_func))
plt.xticks(rotation=45, ha='right') # Rotate labels for better readability
plt.tight_layout() # Adjust layout to prevent labels from overlapping
plt.legend(title='Transfer Type')
plt.savefig("../static/images/tramp/inline_vs_OOB.png")
plt.show()
