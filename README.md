# EdOptimize - An Open Source K-12 Learning Analytics Platform

Brought to you by [Tirth Shah](https://www.linkedin.com/in/tirth-shah-871b3217b/), [Nirmal Patel](https://www.linkedin.com/in/nirmalpatel21/), and [Aditya Sharma](https://www.linkedin.com/in/aditya-sharma-06b529101/) at [Playpower Labs](https://playpowerlabs.com/). For any help with modifying the dashboards, please open an issue or contact Nirmal (nirmal@playpowerlabs.com).

#### Table of Contents

## Introduction
Data from EdTech platforms have a tremendous potential to positively impact the student learning outcomes. EdTech leaders are now realizing that learning analytics data can be used to take decisive actions that make online learning more effective. By using the EdOptimize Platform, we can rapidly begin to observe the implementation of digital learning programs at scale. The data insights from the dashboards in the platform will enable many stakeholders to take evidence based decisions that are aimed at creating an improved online learning experience. 

In the EdOptimize Platform, we are providing 3 extensive data dashboards with different focus areas:

1. **Platform Analytics**: To discover trends and power users in the online learning platform. You can use the user behavior data in this platform to identify actions that can increase user retention and engagement. Live Demo: https://playpowerlabs.shinyapps.io/edopt_platform_analytics/

2. **Curriculum Analytics**: To identify learning patterns in the digital curriculum products. Using this dashboard, you can locate content that needs change and see classroom pacing analytics. You can also look at assessment data, item analysis, and standards performance of the curriculum users. Live Demo: https://playpowerlabs.shinyapps.io/edopt_curriculum_analytics/

3. **Implementation Analytics**: To track the implementation of the digital programs in school districts. This dashboard will help districts make the most out of their online learning programs. Live Demo: https://playpowerlabs.shinyapps.io/edopt_implementation_analytics/

Our platform relies on a commonly seen hierarchical model of educational activity that consists of school districts, schools, classrooms, and ultimately students and teachers within those classrooms. We also have a simple conceptual model of educational programs that has product groups and products where the former is a group of related products such as K-5 Math, 6-8 Science, etc. and the latter represents the individual grade level products. Here is a diagram that relates the conceptual models of the platform with the data dashboards:

Coneptual Model of the EdOptimize Platform![image](https://user-images.githubusercontent.com/830400/116782703-57406e00-aaa8-11eb-8a9f-f48cfe720fcf.png)

All of the analytics dashboards in the EdOptimize Platform work with two commonly used edtech data collection formats:

1. **Event Log Data**: A dataset that typically contains what events or actions students and teachers do over time. [Click here to view a sample](https://docs.google.com/spreadsheets/d/1Zcb4TLAanbNVZORve91Upa0-QL9liZ3G7XNx3Cz4DF4/edit#gid=0)
2. **Item Response Log Data**: A dataset that contains individual responses to test items from the students. [Click here to view a sample](https://docs.google.com/spreadsheets/d/1Zcb4TLAanbNVZORve91Upa0-QL9liZ3G7XNx3Cz4DF4/edit#gid=1857608461)

The data processing scripts in this repository are designed to take the above two data tables and transform them into several smaller tables that will contain the learning analytics.

Data Processing Workflow![image](https://user-images.githubusercontent.com/830400/115994507-e820d080-a5f4-11eb-9c2b-fbbe4067b353.png)

The next section contains detailed descriptions of the dashboards. The section after that walks through the steps to get the dashboards running on your end with your own data.

## Dashboards

Now we will describe each of the dashboards and the various sections withih them. Don't forget to see the live demo!

### Platform Analytics

#### Live Demo: https://playpowerlabs.shinyapps.io/edopt_platform_analytics/

This dashboard allows us to observe the overall usage of an entire digital learning platform. By using this dashboard, we can get insights about how different parts of the platform are being used, which types of content is being used more or less, and discover who are the power users of the platform.

Platform Analytics![image](https://user-images.githubusercontent.com/830400/115994941-b01a8d00-a5f6-11eb-8278-a3a938b7d416.png)

Here are the descriptions of each of the sections in this dashboard:

1. **Overall Usage**: In this section, you can see the high level usage metrics of the platform, usage by different subjects, and a view of how active your users are at large. You can see the various metrics at the user and session level.
2. **Usage Over Time**: Here, you can see several usage over time views for the platform. You can see new versus returning users over time, average session duration over time, and average events per user over time. You can also see leaving and bouncing users in this view. You can see this view at the day, week, or month level for the selected time frame.
3. **Usage by Geography**: This section will let you track platform usage across different geographical areas. Currently, the dashboard supports US states but you can modify the dashboard and make it work with any other geography. You can see the overall usage, usage by grade band, subject, and groups of related products. You can choose from six different usage metrics that you want to display on the map. You also have a choice between seeing a choropleth ploat or a dot plot. The dot plot can be further modified for more precise location level data.
4. **Usage by Products**: In this view, you can see several analytics for a related group of products. You can see the overall number of students and teachers, new and returning users, distribution of the active days per user in the platform, and a comparison of different products within the product group. You can also see the usage for individual products by choosing a deep dive option.
5. **Usage by Content Type**: Here, you can examine which types of content items are being used more or less in the platform. You can view the data by subject, grade band, or product group. For every one of these categories, you can further select the individual subjects, grade bands, and product groups. For example, you can see how different type of content is being used in Math Grade 2 (e.g. Test v/s Videos). You can also see an overall comparison of content type usage to find out what type of content is more or less preferred within your platform.
6. **Leaderboard**: The leaderboard section lets you discover your power users. Here, you can see which school districts are your top users. You can also click on any district in the leaderboard table and look at their usage over time.

### Curriculum Analytics

#### Live Demo: https://playpowerlabs.shinyapps.io/edopt_curriculum_analytics/

Curriculum Analytics toolkit is aimed at understanding how a set of related products (e.g. Math Grades 3-5) are being used at large. This toolkit can provide valuable data to the curriculum teams that can be used to improve various aspects of the digital program that are directly related with its effectiveness.

Figure 3: Curriculum Analytics sections![image](https://user-images.githubusercontent.com/830400/115994734-d2f87180-a5f5-11eb-9d87-5fbcbf3ff56c.png)

Here are the descriptions of each of the sections in this dashboard:

1. **Overall Usage**: In this section, we can see the high level usage summary of a group of related products. We can see the number of users and sessions within each product for a given school year. We can also see the activity level of students and teachers in this view.
2. **Usave Over Time**: Here, we can see several usage over time views for both the overall group of products and for individual products. The individual view allows us to compare how different products within the product group compare to each other for the different metrics. We can see new versus returning users over time, average session duration over time, and average events per user over time. We can also see leaving and bouncing users in this view. We can see this view at the day, week, or month resolution for the selected time frame.
3. **Content Usage**: This section shows us how different types of content items and the specific content items themselves are being utilized by the users of the digital program. We can pick an individual product or look at the data at a group level. We can see a distribution of the % of content accessed by the users, which type of content was used by how many users, and usage over time for the different type of content items (e.g. Video, Quiz, Test, Document, etc.) Finally, we can also see a table that shows us the usage of individual content items along with how much time was spent on them. 
4. **Assessment Analysis**: In this part, we can see data analysis of every assessment present in the individual products. We can see how many students took the assessment, what was their average score, and also the Cronbach's Alpha of the assessment which is a measure of overall reliability of the assessment. We can pick an individual assessment and see the score distribution of that assessment, along with item level data that tell us how many students responded to the item, what was the average % correct for the item, and the point biserial measure which tell us how predictive an invidiau item is of the overall assessment.
5. **Skill Performance**: In this section, we can see average student performance for each skill. We can also see how different skills were assessed in the product over time (we call this skill pacing).
6. **Curriculum Pacing**: Here, you can pick a specific product from the curriculum and look at the usage of that product in the district. You will be able to see plots similar to the Usage Over Time section. Curriculum Pacing plots will allow you to track the pacing of the district for a given product. You can see it for students or teachers, and you can also look at different metrics in the pacing plot. You can also see performance heatmap and performance dotplot for the pacing plots.
7. **Leaderboard**: In this part, we can see which school districts are the top users of the products within this product group. We can select an individual district by clicking on its name in the leaderboard table and see the district usage over time.

### Implementation Analyitcs

Live Demo: https://playpowerlabs.shinyapps.io/edopt_implementation_analytics/

Implementation Analytics toolkit presents a view of how a school district (which is a group of schools) is implementing a digital learning program. Every data view in this toolkit is limited to a single school district.

Figure 2: Implementation Analytics sections![image](https://user-images.githubusercontent.com/830400/115994526-fcfd6400-a5f4-11eb-85e4-6bdd2613796f.png)

Here are the descriptions of the sections in this toolkit:

1. **Overall Usage**: High level summary and distribution of the teacher and student usage across subjects. Average level of activity for teachers and students.
2. **Usage Over Time**: Several views of usage over time at the day, week, and month level. New versus returning users over time, average session duration over time, average events per user over time. Leaving and bouncing users can also be tracked. Raw data for the district is also available.
3. **Curriculum Insights**: here, you can pick a specific product from the curriculum and look at the usage of that product in the district. You will be able to see plots similar to the Usage Over Time section. Curriculum Pacing plots will allow you to track the pacing of the district for a given product. You can see it for students or teachers, and you can also look at different metrics in the pacing plot. You can also see performance heatmap and performance dotplot for the pacing plots.
4. **Leaderboard**: Leaderboard shows which schools and classes are most active in the district. You can look at the leaderboard at the overall or at the product level. For any given class or school in the leaderboard, you can see their usage over time by clicking on the class/school name in the leaderboard table (new versus returning users and average events per user).

## How to Run
Please follow the steps below to run the toolkits on your end.

### Step 1: Understand the data architecture
EdOptimize Toolkits depend on two denormalized data tables:

 1. **Event Log Data**: A data table that collects data about actions of the students and teachers. Every row in this table corresponds to one event. Examples of the events are login, content open, log out, etc.
 2. **Item Response Log Data**: A data table that collects student responses to assessment items. Every row in this table represents a single student response to an item.
 
The column structure of these tables is present in the `data_prep/raw_event_data.RData` and `data_prep/raw_item_response_data.RData` files.

### Step 2: Collect (or simulate) the raw data
You can create your own automated data pipeline that creates the two tables mentioned in the section above. Please ensure that the columns and data types of your data match the samples presented in the 

Optionally, we have also provided a data simulation script `data_prep/raw_data_simulation.R` that simulates the two data tables (i.e. Eveng Log and Item Reponse Log). To simulate the data on your end, you can simply run the `raw_data_simulation.R` and it will generate the RData data files for you.

### Step 3: Run the data processing scripts
Once you have your two data tables ready, you can run the data processing scripts in the `data_prep` folder. There is one script per toolkit. These scripts will generate all of the required RData files for each of the toolkits and copy them in their respective folders. At the end of this step, the toolkits will start running out of the box!

### Step 4: Run the dashboards
To run any toolkit, you need to first prepare the RData files required for the toolkit. Once you have them in place in the toolkit folder, just run the `app.R` file in the individual toolkit folder.

## Data Science Services by Playpower Labs

Playpower Labs is one of the world's leading Educational Data Science consulting company. Our award winning team has worked with many different types of educational data. Examples include event data, assessment data, user behavior data, web analytics data, license and entitlement data, roster data, eText data, item response data, time series data, panel data, hierarchical data, skill and standard data, assignment data, knowledge structure data, school demographic data, and more.

If you need any help with the toolkits, please contact us at `nirmal@playpowerlabs.com`
