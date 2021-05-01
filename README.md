# EdOptimize - An Open Source K-12 Learning Analytics Platform

Brought to you by [Tirth Shah](https://www.linkedin.com/in/tirth-shah-871b3217b/), [Nirmal Patel](https://www.linkedin.com/in/nirmalpatel21/), and [Aditya Sharma](https://www.linkedin.com/in/aditya-sharma-06b529101/) at [Playpower Labs](https://playpowerlabs.com/). For any help with modifying the dashboards, please open an issue or contact Nirmal (nirmal@playpowerlabs.com).

#### Live Demo Links

Platform Analytics: https://playpowerlabs.shinyapps.io/edopt_platform_analytics/

Curriculum Analytics: https://playpowerlabs.shinyapps.io/edopt_curriculum_analytics/

Implementation Analytics: https://playpowerlabs.shinyapps.io/edopt_implementation_analytics/

#### Table of Contents

- [Introduction](#introduction)
- [Dashboards](#dashboards)
  - [Platform Analytics](#platform-analytics)
  - [Curriculum Analytics](#curriculum-analytics)
  - [Implementation Analytics](#implementation-analyitcs)
- [How to Run](#how-to-run)
- [About Us](#about-us)

## Introduction
Data from EdTech platforms have a tremendous potential to positively impact the student learning outcomes. EdTech leaders are now realizing that learning analytics data can be used to take decisive actions that make online learning more effective. By using the EdOptimize Platform, we can rapidly begin to observe the implementation of digital learning programs at scale. The data insights from the EdOptimize Platform can enable education stakeholders to take evidence based decisions that are aimed at creating improved online learning experiences. 

EdOptimize Platform is a collection of 3 extensive data dashboards. These dashboards contain many actionable learning analytics that we have designed from our years of work with various school districts in the US.

![image](https://user-images.githubusercontent.com/830400/116783550-e3ed2b00-aaac-11eb-8171-395c0f1c43a6.png)

Here are the brief descriptions of each of the dashboards:

1. **Platform Analytics**: To discover trends and power users in the online learning platform. You can use the user behavior data in this platform to identify actions that can increase user retention and engagement. See the dashboard in action here: https://playpowerlabs.shinyapps.io/edopt_platform_analytics/

2. **Curriculum Analytics**: To identify learning patterns in the digital curriculum products. Using this dashboard, you can locate content that needs change and see classroom pacing analytics. You can also look at assessment data, item analysis, and standards performance of the curriculum users. See the dashboard in action here: https://playpowerlabs.shinyapps.io/edopt_curriculum_analytics/

3. **Implementation Analytics**: To track the implementation of the digital programs in school districts. This dashboard will help districts make the most out of their online learning programs. See the dashboard in action here: https://playpowerlabs.shinyapps.io/edopt_implementation_analytics/

Our platform relies on a commonly seen hierarchical model of educational activity that consists of school districts, schools, classrooms, and ultimately students and teachers within those classrooms. We also have a simple conceptual model of educational programs that has product groups and products where the former is a group of related products such as K-5 Math, 6-8 Science, etc. and the latter represents the individual grade level products. Here is a diagram that relates the conceptual models of the platform with the data dashboards:

Coneptual Model of the EdOptimize Platform![image](https://user-images.githubusercontent.com/830400/116783565-f36c7400-aaac-11eb-9d1a-475420cf89fc.png)

The next section contains detailed descriptions of the dashboards. The section after that walks through the steps to get the dashboards running on your end with your own data.

## Dashboards

Now we will describe each of the dashboards and the various sections withih them. Don't forget to see the live demo!

### Platform Analytics

#### Live Demo: https://playpowerlabs.shinyapps.io/edopt_platform_analytics/

This dashboard allows us to observe the overall usage of an entire digital learning platform. By using this dashboard, we can get insights about how different parts of the platform are being used, which types of content is being used more or less, and discover who are the power users of the platform.

![image](https://user-images.githubusercontent.com/830400/115994941-b01a8d00-a5f6-11eb-8278-a3a938b7d416.png)

Here are the descriptions of each of the sections in this dashboard:

1. **Overall Usage**: In this section, you can see the high level usage metrics of the platform, usage by different subjects, and a view of how active your users are at large. You can see the various metrics at the user and session level.
2. **Usage Over Time**: Here, you can see several usage over time views for the platform. You can see new versus returning users over time, average session duration over time, and average events per user over time. You can also see leaving and bouncing users in this view. You can see this view at the day, week, or month level for the selected time frame.
3. **Usage by Geography**: This section will let you track platform usage across different geographical areas. Currently, the dashboard supports US states but you can modify the dashboard and make it work with any other geography. You can see the overall usage, usage by grade band, subject, and groups of related products. You can choose from six different usage metrics that you want to display on the map. You also have a choice between seeing a choropleth ploat or a dot plot. The dot plot can be further modified for more precise location level data.
4. **Usage by Products**: In this view, you can see several analytics for a related group of products. You can see the overall number of students and teachers, new and returning users, distribution of the active days per user in the platform, and a comparison of different products within the product group. You can also see the usage for individual products by choosing a deep dive option.
5. **Usage by Content Type**: Here, you can examine which types of content items are being used more or less in the platform. You can view the data by subject, grade band, or product group. For every one of these categories, you can further select the individual subjects, grade bands, and product groups. For example, you can see how different type of content is being used in Math Grade 2 (e.g. Test v/s Videos). You can also see an overall comparison of content type usage to find out what type of content is more or less preferred within your platform.
6. **Leaderboard**: The leaderboard section lets you discover your power users. Here, you can see which school districts are your top users. You can also click on any district in the leaderboard table and look at their usage over time.

### Curriculum Analytics

#### Live Demo: https://playpowerlabs.shinyapps.io/edopt_curriculum_analytics/

This dashboard is aimed at understanding how a set of related products (e.g. Math Grades 3-5) are being used at large. The data in this dashboard can provide valuable insights to the curriculum teams that want to improve various aspects of the digital program. The analytics in this dashboard always focus on a specific school year.

![image](https://user-images.githubusercontent.com/830400/115994734-d2f87180-a5f5-11eb-9d87-5fbcbf3ff56c.png)

Here are the descriptions of each of the sections in this dashboard:

1. **Overall Usage**: In this section, you can see the high level usage summary of a group of related products. You can see the number of users and sessions within each product. We can also see the activity level of students and teachers in this view.
2. **Usave Over Time**: Here, you can see several usage over time views for both the overall group of products and for individual products. The individual view will allow you to compare how different products within the product group compare to each other. You can see new versus returning users over time, average session duration over time, and average events per user over time in this view. You can also see leaving and bouncing users over time in this view, and the data can be seen at the level of day, week, or month for the selected time frame.
3. **Content Usage**: This section will show you how different types of content items and the specific content items themselves are being utilized by the online learners. You can pick an individual product or look at the data at the product group level. You can see the distribution of the % content utilization, which type of content was used by how many users, and usage over time for the different type of content items (e.g. Videos, Quizzes, Tests, Documents, etc.) Finally, you can also see a table that shows the usage of individual content items along with how much time was spent on them.
4. **Assessment Analysis**: In this part, you can see data analysis of every assessment present in the individual products. You can see how many students took each assessment, what was their average score, and also the Cronbach's Alpha of the assessment which is a measure of overall reliability of the assessment. You can pick an individual assessment and see the score distribution of that assessment, along with item level data that tell us how many students responded to each of the items, what was their average % correct, and the point biserial measure of the item which tell us how predictive the individual item is of the overall assessment. If you find assessments with low Cronbach's Alpha and items with low point biserial, they should be inspected further for their validity.
5. **Skill Performance**: In this section, you can see the average student performance on each skill that is part of a product. You can also see how different skills were assessed over time during the school year.
6. **Curriculum Pacing**: Curriculum Pacing charts are award-winning data visualizations that show how students go through a curriculum over time. [Click here to read our research on the Curriculum Pacing](https://link.springer.com/chapter/10.1007/978-3-319-91464-0_38). In this section, you can pick a specific product from the curriculum and look at the pacing patterns of the students. You can see the pacing patterns for students or teachers. You can choose between a heatmap or dot plot representation of the pacing visualization. There are several metrics that can be plotted on these visualizations.
7. **Leaderboard**: In this part of the dashboard, you can see which school districts are the top users of the products within this product group. You can select an individual district by clicking on its name in the leaderboard table and see the district's product usage over time.

### Implementation Analyitcs

#### Live Demo: https://playpowerlabs.shinyapps.io/edopt_implementation_analytics/

This dashboard presents a view of how a school district (which is a group of schools) is implementing a digital learning program. Every data view in this dashboard is limited to a single school district. The various patterns of digital learning within the district can empower many stakeholders to make online learning more effective for students.

![image](https://user-images.githubusercontent.com/830400/115994526-fcfd6400-a5f4-11eb-85e4-6bdd2613796f.png)

Here are the descriptions of each of the sections in this dashboard:

1. **Overall Usage**: This section will give you a high level summary and distribution of the teacher and student usage across subjects within the district. You will also see analytics around the average level of activity for teachers and students.
2. **Usage Over Time**: Here, you can see several views of the usage over time at the day, week, and month level. You can see new versus returning users over time, average session duration over time, and average events per user over time for the different time periods. Leaving and bouncing users can also be tracked here similar to other dashboards. The raw data for the district are also available in this section.
3. **Curriculum Insights**: Here, you can pick a specific product that the district is using, and closely look at the usage of that product. You will be able to see plots similar to the Usage Over Time section. Curriculum Pacing plots will allow you to track the pacing patterns within the the district for a given product. For more description fo the pacing plots, see the Curriculum Pacing section of the Curriculum Analytics dashboard.
4. **Leaderboard**: The leaderboard will show you which schools and classes are most active in the district. You can look at the leaderboard at the overall level or at the product level. For any given class or school in the leaderboard, you can see their usage over time by clicking on the class/school name in the leaderboard table.

## How to Run

Please follow the steps below to run the dashboards on your end.

### Step 1: Understand the data architecture

All of the analytics dashboards in the EdOptimize Platform work with two commonly used edtech data collection formats:

1. **Event Log Data**: A dataset that typically contains what events or actions students and teachers do over time. [Click here to view a sample](https://docs.google.com/spreadsheets/d/1Zcb4TLAanbNVZORve91Upa0-QL9liZ3G7XNx3Cz4DF4/edit#gid=0)
2. **Item Response Log Data**: A dataset that contains individual responses to test items from the students. [Click here to view a sample](https://docs.google.com/spreadsheets/d/1Zcb4TLAanbNVZORve91Upa0-QL9liZ3G7XNx3Cz4DF4/edit#gid=1857608461)

### Step 2: Collect (or simulate) the raw data
You can create your own automated data pipeline that creates the two tables mentioned in the section above. Please ensure that the columns and data types of your data match the data samples presented. If you do not have all of the data points presented in the tables above, try to put in some dummy values. If you want to modify these dashboards, please open an issue. If you need professional help, see our contact details at the top or bottom.

Optionally, we have also provided a data simulation script `data_prep/raw_data_simulation.R` that simulates the two data tables (i.e. Eveng Log and Item Reponse Log). To simulate the data on your end, you can simply run the `raw_data_simulation.R` and it will generate the required RData data containing the raw data for you.

### Step 3: Run the data processing scripts

The data processing scripts in this repository are designed to take the above two raw data tables and transform them into several smaller tables that will contain the learning analytics. Here is the 

Data Processing Workflow![image](https://user-images.githubusercontent.com/830400/115994507-e820d080-a5f4-11eb-9c2b-fbbe4067b353.png)

Once you have your two data tables ready (simulated or otherwise), you can run the data processing scripts individual dashboard folders. There is one script per dashboard. These scripts will generate all of the required RData files for each of the dashboards and place them in their respective folders. Here are the scripts for each of the dashboards:

1. Platform Analytics: `implementation_analytics/data/implementation_analytics_data_prep.R`
2. Curriculum Analytics: `implementation_analytics/data/implementation_analytics_data_prep.R`
3. Implementation Analytics: `implementation_analytics/data/implementation_analytics_data_prep.R`

At the end of this step, the dashboards will start running out of the box!

### Step 4: Run the dashboards
To run any dashboard after you have generated its data, simple go to the dashboard folder and run `app.R` file.

## About Us

We are a team of dedicated Learning Analytics experts at Playpower Labs.

Playpower Labs is one of the world's leading EdTech consulting company. Our award winning research team has worked with many different types of educational data. Examples include event data, assessment data, user behavior data, web analytics data, license and entitlement data, roster data, eText data, item response data, time series data, panel data, hierarchical data, skill and standard data, assignment data, knowledge structure data, school demographic data, and more.

If you need any help with this platform or any other project, please contact our Chief Data Scientist Nirmal Patel at nirmal@playpowerlabs.com He will be happy to have a conversation with you!
