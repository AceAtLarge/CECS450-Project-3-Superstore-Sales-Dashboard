# CECS450-Project-3-Superstore-Sales-Dashboard
A fully interactive dashboard using R Shiny made in RStudio

Details

**Superstore Sales Dashboard Project (R Shiny)**

**Project Title: Interactive Superstore Sales Dashboard using R Shiny**

**Project Goal**

In this project, you will build a **fully interactive dashboard** using R Shiny to explore and analyze sales data from a retail company.

Your dashboard will allow users to:

-   filter data dynamically
-   view key performance metrics
-   explore maps and charts
-   identify trends and patterns

This project simulates a **real-world business analytics task**.

**What You Will Learn**

By completing this project, you will learn how to:

-   build interactive dashboards using **R Shiny**
-   clean and prepare real datasets
-   create filters and reactive logic
-   build interactive charts using **ggplot2 + plotly**
-   summarize data using **dplyr**
-   present insights clearly

**Dataset**

You will use the **Superstore dataset**, which includes:

-   Order Date
-   Ship Date
-   Sales
-   Profit
-   Discount
-   State
-   Region
-   Category
-   Customer Segment
-   Ship Mode

**Required Packages**

Install and load the following packages:

library(shiny)  
library(dplyr)  
library(ggplot2)  
library(plotly)  
library(readr)  
library(lubridate)  
library(scales)  
library(DT)

**PROJECT REQUIREMENTS**

Your dashboard must include **ALL components below**.

1.  **Data Preparation**

You must:

-   load the dataset into R
-   Convert date columns into the proper date format
-   create state abbreviations (for map visualization)

2.  **Sidebar Filters**

Your app must allow users to filter by:

-   Region
-   Department
-   Customer Segment
-   Ship Mode
-   Discount Range
-   Order Date Range

3.  **KPI Summary Cards**

Display the following metrics:

-   Total Sales
-   Total Profit
-   Number of Orders
-   Average Discount

4.  **Dashboard Tabs**

Your app must include these tabs:

1.  **Map**

-   U.S. map showing **Sales by State**

2.  **Category Analysis**

-   Bar chart showing **Sales by Category**

3.  **Profit vs Discount**

-   Scatter plot showing:

-   Discount (x-axis)
-   Profit (y-axis)

4.  **Monthly Trend**

-   Line chart showing **Sales over time**

5.  **Top States Table**

-   Table showing:

-   Total Sales
-   Total Profit
-   Average Discount
-   Orders

6.  **Interactivity (IMPORTANT)**

All charts, tables, and KPIs must:

-   update automatically when filters change
-   use **reactive programming**

Follow these steps to complete your project.

**Step 1: Set Up Your Project**

-   Open RStudio
-   Create a new project folder
-   Create a file called app.R
-   Place your dataset in the folder

**Step 2: Load Packages**

Load all required libraries at the top of your script.

**Step 3: Load and Clean Data**

-   Read the dataset
-   Convert:

-   Order Date
-   Ship Date

-   Create **state abbreviations**

**Step 4: Build UI (User Interface)**

Your UI must include:

-   Title
-   Sidebar (filters)
-   Main panel
-   KPI cards
-   Tabbed layout

**Step 5: Add Filters**

Add dropdowns, sliders, and date inputs for filtering.

**Step 6: Create Reactive Data**

Create a reactive dataset that:

-   starts with full data
-   applies filters based on user input

**Step 7: Build KPI Outputs**

Calculate and display:

-   total sales
-   total profit
-   number of rows (orders)
-   average discount

**Step 8: Create Visualizations**

**Map**

-   Sales by state
-   interactive hover labels

**Category Chart**

-   bar chart of total sales

**Profit vs Discount**

-   scatter plot
-   add trend line

**Monthly Trend**

-   group data by month
-   create line chart

**Step 9: Create Table**

Build an interactive table showing:

-   state-level summaries
-   formatted values (currency, percentages)

**Step 10: Run Your App**

Add this at the end:

shinyApp(ui = ui, server = server)

Click **Run App**.

**Step 11: Test Everything**

Make sure:

-   filters work
-   charts update
-   KPIs update
-   no errors occur

**ANALYSIS (VERY IMPORTANT)**

After building your dashboard, answer these questions:

1.  Which state has the highest sales?
2.  Which category performs best?
3.  Does higher discount reduce profit?
4.  When are sales highest during the year?
5.  Which regions perform better?

**FINAL SUBMISSION**

Submit the following:

1.  **R Shiny App (** app.R)
2.  **Report (PDF or Word)**

Include:

-   project description
-   dataset explanation
-   dashboard features
-   key insights (at least 5)
-   challenges

3.  **Screenshots**

Include: full dashboard and each tab

**EXTRA CREDIT (Optional)**

You may add:

-   download button
-   additional charts
-   advanced styling
-   extra filters
-   new KPIs

**IMPORTANT NOTES**

-   Your dashboard must be **fully functional**
-   All filters must work correctly
-   You must understand your own code
-   Your report must include **real insights**, not just screenshots
