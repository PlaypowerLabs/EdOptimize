# EdOptimize - An Open Source K-12 Learning Analytics Platform

LICENSE NOTE: CURRENTLY, THIS PLATFORM IS LICENSED GPLv3. ALL MODIFICATIONS/COPIES OF THIS PLATFORM MUST HAVE GPLv3 LICENSE AND MUST DISCLOSE ALL OF THEIR SOURCE CODE. If you want to use EdOptimize for your organization, please contact us and we will help you with that. Our contact details are given at the bottom of the page.

Brought to you by the data science team at [Playpower Labs](https://playpowerlabs.com/). Authored by [Tirth Shah](https://www.linkedin.com/in/tirth-shah-871b3217b/) and designed by [Nirmal Patel](https://www.linkedin.com/in/nirmalpatel21/) and [Aditya Sharma](https://www.linkedin.com/in/aditya-sharma-06b529101/).

For any help with modifying the dashboards, please open an issue.

#### Live Demo Links

![image](https://user-images.githubusercontent.com/830400/116785618-354ee780-aab8-11eb-97a8-031f7e146376.png)

https://playpowerlabs.shinyapps.io/edopt_platform_analytics/

![image](https://user-images.githubusercontent.com/830400/116785631-4566c700-aab8-11eb-9633-3fe171f4aa57.png)

https://playpowerlabs.shinyapps.io/edopt_curriculum_analytics/

![image](https://user-images.githubusercontent.com/830400/116785636-4861b780-aab8-11eb-8527-c89995e0f58c.png)

https://playpowerlabs.shinyapps.io/edopt_implementation_analytics/

#### Table of Contents

- [Introduction](#introduction)
- [Dashboards](#dashboards)
  - [Platform Analytics](#platform-analytics)
  - [Curriculum Analytics](#curriculum-analytics)
  - [Implementation Analytics](#implementation-analytics)
- [Data Architecture](#data-architecture)
- [How to Run](#how-to-run)
- [About Playpower Labs](#about-playpower-labs)

## Introduction
Data from EdTech platforms have a tremendous potential to positively impact student learning outcomes. EdTech leaders are now realizing that learning analytics data can be used to take decisive actions that make online learning more effective. By using the EdOptimize Platform, we can rapidly begin to observe the implementation of digital learning programs at scale. The data insights from the EdOptimize Platform can enable education stakeholders to make evidence-based decisions that are aimed at creating improved online learning experiences. 

EdOptimize Platform is a collection of 3 extensive data dashboards. These dashboards contain many actionable learning analytics that we have designed from our years of work with various school districts in the US.

![image](https://user-images.githubusercontent.com/830400/117278121-19639100-ae7e-11eb-95f3-ff7e7654a900.png)

Here are the brief descriptions of each of the dashboards:

1. **Platform Analytics**: This dashboard aims at giving EdTech product teams data about the trends and power users in their online learning platform. They can use the user behavior data from their platform to identify actions that can increase user retention and engagement. See the dashboard in action here: https://playpowerlabs.shinyapps.io/edopt_platform_analytics/

2. **Curriculum Analytics**: This dashboard is designed to help curriculum teams identify learning patterns in their digital curriculum products. Using this dashboard, the curriculum designers can locate content that needs change and see classroom pacing analytics. Additionally, we can also look at assessment data, item analysis, and standards performance of the students. See the dashboard in action here: https://playpowerlabs.shinyapps.io/edopt_curriculum_analytics/

3. **Implementation Analytics**: This dashboard is designed to track the implementation of digital programs in school districts. This dashboard will help districts make the most out of their online learning programs. The data in this dashboard can be extremely helpful for both district leaders and success managers who want to ensure a smooth and effective digital program implementation.. See the dashboard in action here: https://playpowerlabs.shinyapps.io/edopt_implementation_analytics/

Our platform relies on a commonly seen hierarchical model of educational activity that consists of school districts, schools, classrooms, and ultimately students and teachers within those classrooms. We also have a simple conceptual model of educational programs that has product groups and products where the former is a group of related products such as K-5 Math, 6-8 Science, etc. and the latter represents the individual grade level products. Here is a diagram that relates the conceptual models of the platform with the data dashboards:

Conceptual Model of the EdOptimize Platform![image](https://user-images.githubusercontent.com/830400/116783565-f36c7400-aaac-11eb-9d1a-475420cf89fc.png)

The next section contains detailed descriptions of the dashboards. The section after that walks through the steps to get the dashboards running on your end with your own data.

## Dashboards

Now we will describe each of the dashboards and the various sections within them. Don't forget to see the live demo!

### Platform Analytics

![image](https://user-images.githubusercontent.com/830400/116785618-354ee780-aab8-11eb-97a8-031f7e146376.png)

#### Live Demo: https://playpowerlabs.shinyapps.io/edopt_platform_analytics/

This dashboard allows us to observe the overall usage of an entire digital learning platform. By using this dashboard, we can get insights about how different parts of the platform are being used, which types of content is being used more or less, and discover who are the power users of the platform.

![image](https://user-images.githubusercontent.com/830400/115994941-b01a8d00-a5f6-11eb-8278-a3a938b7d416.png)

Here are the descriptions of each of the sections in this dashboard:

1. **Overall Usage**: In this section, you can see the high-level usage metrics of the platform, usage by different subjects, and a view of how active your users are at large. You can see the various metrics at the user and session-level.
2. **Usage Over Time**: Here, you can see several usage over time views for the platform. You can see new versus returning users over time, average session duration over time, and average events per user over time. You can also see leaving and bouncing users in this view. You can see this view at the day, week, or month level for the selected time frame.
3. **Usage by Geography**: This section will let you track platform usage across different geographical areas. Currently, the dashboard supports US states but you can modify the dashboard and make it work with any other geography. You can see the overall usage, usage by grade band, subject, and groups of related products. You can choose from six different usage metrics that you want to display on the map. You also have a choice between seeing a choropleth plot or a dot plot. The dot plot can be further modified for more precise location-level data.
4. **Usage by Products**: In this view, you can see analytics for a related group of products. You can see the overall number of students and teachers, new and returning users, distribution of the active days per user in the platform, and a comparison of different products within the product group. You can also see the usage for individual products by choosing a deep dive option.
5. **Usage by Content-Type**: Here, you can examine which types of content items are being used more or less in the platform. You can view the data by subject, grade band, or product group. For every one of these categories, you can further select the individual subjects, grade bands, and product groups. For example, you can see how different type of content is being used in Math Grade 2 (e.g. Test v/s Videos). You can also see an overall comparison of content type usage to find out what type of content is more or less preferred within your platform.
6. **Leaderboard**: The leaderboard section lets you discover your power users. Here, you can see which school districts are your top users. You can also click on any district in the leaderboard table and look at their usage over time.

### Curriculum Analytics

![image](https://user-images.githubusercontent.com/830400/116785631-4566c700-aab8-11eb-9633-3fe171f4aa57.png)

#### Live Demo: https://playpowerlabs.shinyapps.io/edopt_curriculum_analytics/

This dashboard is aimed at understanding how a set of related products (e.g. Math Grades 3-5) are being used at large. The data in this dashboard can provide valuable insights to the curriculum teams that want to improve various aspects of the digital program. The analytics in this dashboard always focus on a specific school year.

![image](https://user-images.githubusercontent.com/830400/115994734-d2f87180-a5f5-11eb-9d87-5fbcbf3ff56c.png)

Here are the descriptions of each of the sections in this dashboard:

1. **Overall Usage**: In this section, you can see the high-level usage summary of a group of related products. You can see the number of users and sessions within each product. We can also see the activity level of students and teachers in this view.
2. **Usage Over Time**: Here, you can see several usage over time views for both the overall group of products and for individual products. The individual view will allow you to compare how different products within the product group compare to each other. You can see new versus returning users over time, average session duration over time, and average events per user over time in this view. You can also see leaving and bouncing users over time in this view, and the data can be seen at the level of the day, week, or month for the selected time frame.
3. **Content Usage**: This section will show you how different types of content items and the specific content items themselves are being utilized by the online learners. You can pick an individual product or look at the data at the product group level. You can see the distribution of the % content utilization, which type of content was used by how many users, and usage over time for the different types of content items (e.g. Videos, Quizzes, Tests, Documents, etc.) Finally, you can also see a table that shows the usage of individual content items along with how much time was spent on them.
4. **Assessment Analysis**: In this part, you can see data analysis of every assessment present in the individual products. You can see how many students took each assessment, what was their average score, and also the Cronbach's Alpha of the assessment which is a measure of the overall reliability of the assessment. You can pick an individual assessment and see the score distribution of that assessment, along with item-level data that tell us how many students responded to each of the items, what was their average % correct, and the point biserial measure of the item which tell us how predictive the individual item is of the overall assessment. If you find assessments with low Cronbach's Alpha and items with low point biserial, they should be inspected further for their validity.
5. **Skill Performance**: In this section, you can see the average student performance on each skill that is part of a product. You can also see how different skills were assessed over time during the school year.
6. **Curriculum Pacing**: Curriculum Pacing charts are award-winning data visualizations that show how students go through a curriculum over time. [Click here to read our research on Curriculum Pacing](https://link.springer.com/chapter/10.1007/978-3-319-91464-0_38). In this section, you can pick a specific product from the curriculum and look at the pacing patterns of the students. You can see the pacing patterns for students or teachers. You can choose between a heatmap or dot plot representation of the pacing visualization. Several metrics can be plotted on these visualizations.
7. **Leaderboard**: In this part of the dashboard, you can see which school districts are the top users of the products within this product group. You can select an individual district by clicking on its name in the leaderboard table and see the district's product usage over time.

### Implementation Analytics

![image](https://user-images.githubusercontent.com/830400/116785636-4861b780-aab8-11eb-8527-c89995e0f58c.png)

#### Live Demo: https://playpowerlabs.shinyapps.io/edopt_implementation_analytics/

This dashboard presents a view of how a school district (which is a group of schools) is implementing a digital learning program. Every data view in this dashboard is limited to a single school district. The various patterns of digital learning within the district can empower many stakeholders to make online learning more effective for students.

![image](https://user-images.githubusercontent.com/830400/117278198-2d0ef780-ae7e-11eb-8fb9-4ba36c1c6fd7.png)

Here are the descriptions of each of the sections in this dashboard:

1. **Overall Usage**: This section will give you a high-level summary and distribution of the teacher and student usage across subjects within the district. You will also see analytics around the average level of activity for teachers and students.
2. **Usage Over Time**: Here, you can see several views of the usage over time at the day, week, and month level. You can see new versus returning users over time, average session duration over time, and average events per user over time for the different periods. Leaving and bouncing users can also be tracked here similar to other dashboards. The raw data for the district are also available in this section.
3. **Curriculum Insights**: Here, you can pick a specific product that the district is using, and closely look at the usage of that product. You will be able to see plots similar to the Usage Over Time section. Curriculum Pacing plots will allow you to track the pacing patterns within the district for a given product. For more description of the pacing plots, see the Curriculum Pacing section of the Curriculum Analytics dashboard.
4. **Leaderboard**: The leaderboard will show you which schools and classes are most active in the district. You can look at the leaderboard at the overall level or the product level. For any given class or school in the leaderboard, you can see their usage over time by clicking on the class/school name in the leaderboard table.

## Data Architecture

All of the analytics dashboards in the EdOptimize Platform work with two commonly used edtech data collection formats:

1. **Online Platform Data**: A dataset that typically contains what actions students and teachers do over time. [Click here to view a sample](https://docs.google.com/spreadsheets/d/1Zcb4TLAanbNVZORve91Upa0-QL9liZ3G7XNx3Cz4DF4/edit#gid=0) This dataset is stored in an event log format which is also used in Business Process Mining domain.
2. **Assessment Data**: A dataset that contains individual responses to assessment items from the students. [Click here to view a sample](https://docs.google.com/spreadsheets/d/1Zcb4TLAanbNVZORve91Upa0-QL9liZ3G7XNx3Cz4DF4/edit#gid=1857608461)

## How to Run

Please follow the steps below to run the dashboards on your end.

### Step 1: Collect (or simulate) the raw data
You can create your automated data pipeline that creates the two tables mentioned in the section above. Please ensure that the columns and data types of your data match the data samples presented. If you do not have all of the data points presented in the tables above, try to put in some dummy values. If you want to modify these dashboards, please open an issue. If you need professional help, see our contact details at the top or bottom.

Optionally, we have also provided a data simulation script `data_prep/raw_data_simulation.R` that simulates the two data tables (i.e. Event Log and Item Response Log). To simulate the data on your end, you can simply run the `raw_data_simulation.R` and it will generate the required RData data containing the raw data for you.

### Step 2: Run the data processing scripts

The data processing scripts in this repository are designed to take the above two raw data tables and transform them into several smaller tables that will contain the learning analytics. Here is the 

Data Processing Workflow![diagram1](https://user-images.githubusercontent.com/830400/120097073-35d8ad80-c14c-11eb-8437-1c48ee1c52aa.png)

Once you have your two data tables ready (simulated or otherwise), you can run the data processing scripts individual dashboard folders. There is one script per dashboard. These scripts will generate all of the required RData files for each of the dashboards and place them in their respective folders. Here are the scripts for each of the dashboards:

1. Platform Analytics: `platform_analytics/data/platform_analytics_data_prep.R`
2. Curriculum Analytics: `curriculum_analytics/data/curriculum_analytics_data_prep.R`
3. Implementation Analytics: `implementation_analytics/data/implementation_analytics_data_prep.R`

At the end of this step, the dashboards will start running out of the box!

### Step 3: Run the dashboards
To run any dashboard after you have generated its data, go to the specific dashboard folder and run the `app.R` file.

## About Playpower Labs

Playpower Labs is one of the world's leading EdTech consulting companies. Our award-winning research team has worked with many different types of educational data. Examples include event data, assessment data, user behavior data, web analytics data, license and entitlement data, roster data, eText data, item response data, time-series data, panel data, hierarchical data, skill and standard data, assignment data, knowledge structure data, school demographic data, and more.

If you need a professional help with this platform or any other EdTech data project, please contact our Chief Data Scientist Nirmal Patel at nirmal@playpowerlabs.com He will be happy to have a conversation with you!
