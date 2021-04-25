# EdOptimize Learning Analytics Toolkits
### For K-12 Digital Learning Platforms

Data from EdTech platforms have a tremendous potential to positively impact the student learning outcomes. EdTech leaders are now realizing that learning analytics data can be used to take decisive actions that make online learning more effective. By using the EdOptimize Toolkits, product, curriculum, and customer support teams can rapidly begin to observe the implementation of digital learning programs at scale. The data insights from these toolkits will enable many stakeholders to take evidence based decisions that are aimed at creating an engaging and effective online learning experience. 

We are providing 3 learning analytics toolkits with different focus areas:

1. **Implementation Analytics**: Track the implementation of the digital programs in a group of learners (e.g. a school district). Help them make the most out of their programs by knowing where they are struggling.

2. **Curriculum Analytics**: Identify patterns in the digital curriculum products. Locate content that needs change and see classroom pacing analytics. Look at assessment and standards performance of the program users.

3. **Platform Analytics**: Discover trends and power users in the online learning platform. Use the user behavior data to tweak the online platform and increase user retention and engagement.

All of the above toolkits work with a common data format. Here is a simplified diagram of how things work:

Figure 1: How things work at a high level![image](https://user-images.githubusercontent.com/830400/115012160-1bfd4700-9ecd-11eb-9882-1ba3efbfe5dc.png)

Below, we have provided more details of each of the toolkits. As indicated in the diagram above, these toolkits are based on a common data format. This data format is described at the end. If you can get your data in the given format, all of the toolkits will start working for you. If you do not have all of the data, you might have to modify the R code.

## Implementation Analyitcs

Live Demo: https://playpowerlabs.shinyapps.io/edopt_implementation_analytics/

Implementation Analytics toolkit presents a view of how a school district (which is a group of schools) is implementing a digital learning program. Every data view in this toolkit is limited to a single school district. Here are the sections in this toolkit:

1. **Overall Usage**: High level summary and distribution of the teacher and student usage across subjects. Average level of activity for teachers and students.
2. **Usage Over Time**: Several views of usage over time at the day, week, and month level. New versus returning users over time, average session duration over time, average events per user over time. Leaving and bouncing users can also be tracked. Raw data for the district is also available.
3. **Curriculum Insights**: here, you can pick a specific product from the curriculum and look at the usage of that product in the district. You will be able to see plots similar to the Usage Over Time section. Curriculum Pacing plots will allow you to track the pacing of the district for a given product. You can see it for students or teachers, and you can also look at different metrics in the pacing plot. You can also see performance heatmap and performance dotplot for the pacing plots.
4. **Leaderboard**: Leaderboard shows which schools and classes are most active in the district. You can look at the leaderboard at the overall or at the product level. For any given class or school in the leaderboard, you can see their usage over time by clicking on the class/school name in the leaderboard table (new versus returning users and average events per user).

## Curriculum Analytics

Live Demo: https://playpowerlabs.shinyapps.io/edopt_curriculum_analytics/

Curriculum Analytics toolkit is aimed at understanding how a set of related products (e.g. Math Grades 3-5) are being used at large. This toolkit can provide valuable data to the curriculum teams that can be used to improve various aspects of the digital program that are directly related with its effectiveness. Here are the sections in this toolkit:

1. **Overall Usage**: In this section, we can see the high level usage summary of a group of related products. We can see the number of users and sessions within each product for a given school year. We can also see the activity level of students and teachers in this view.
2. **Usave Over Time**: Here, we can see several usage over time views for both the overall group of products and for individual products. The individual view allows us to compare how different products within the product group compare to each other for the different metrics. We can see new versus returning users over time, average session duration over time, and average events per user over time. We can also see leaving and bouncing users in this view. We can see this view at the day, week, or month resolution for the selected time frame.
3. **Content Usage**: This section shows us how different types of content items and the specific content items themselves are being utilized by the users of the digital program. We can pick an individual product or look at the data at a group level. We can see a distribution of the % of content accessed by the users, which type of content was used by how many users, and usage over time for the different type of content items (e.g. Video, Quiz, Test, Document, etc.) Finally, we can also see a table that shows us the usage of individual content items along with how much time was spent on them. 
4. **Assessment Analysis**: In this part, we can see data analysis of every assessment present in the individual products. We can see how many students took the assessment, what was their average score, and also the Cronbach's Alpha of the assessment which is a measure of overall reliability of the assessment. We can pick an individual assessment and see the score distribution of that assessment, along with item level data that tell us how many students responded to the item, what was the average % correct for the item, and the point biserial measure which tell us how predictive an invidiau item is of the overall assessment.
5. **Skill Performance**: In this section, we can see average student performance for each skill. We can also see how different skills were assessed in the product over time (we call this skill pacing).
6. **Curriculum Pacing**: Here, you can pick a specific product from the curriculum and look at the usage of that product in the district. You will be able to see plots similar to the Usage Over Time section. Curriculum Pacing plots will allow you to track the pacing of the district for a given product. You can see it for students or teachers, and you can also look at different metrics in the pacing plot. You can also see performance heatmap and performance dotplot for the pacing plots.
7. **Leaderboard**: In this part, we can see which school districts are the top users of the products within this product group. We can select an individual district by clicking on its name in the leaderboard table and see the district usage over time.

## Platform Analytics

Live Demo: https://playpowerlabs.shinyapps.io/edopt_platform_analytics/

This toolkit allows us to observer the overall usage of the entire digital learning platform. We can get insights about how different parts of the platform are being used, which types of content is being used more, and who are the power users. Here are the sections in this toolkit:

1. **Overall Usage**: In this section, you can see the high level usage metrics of the platform, usage by subject, and a view of how active your users are at large. You can see the metrics by both number of users and sessions.
2. **Usage Over Time**: Here, we can see several usage over time views for the whole platform. We can see new versus returning users over time, average session duration over time, and average events per user over time. We can also see leaving and bouncing users in this view. We can see this view at the day, week, or month resolution for the selected time frame.
3. **Usage by Geography**: This section lets us track platform usage over the geographical areas. Currently, the dashboard supports US states. You can see the overall usage, usage by grade band, subject, and groups of related products. You can choose from six different usage metrics that you want to see on the map. You can chooes between a choropleth and a dot plot.
4. **Usage by Products**: In this view, we can see several analytics for related group of products. We can see the overall number of students and teachers, new and returning users, distribution of the active days per user in the platform, and a comparison of different products within the product group. We can also see the usage plots for individual products by choosing a deep dive option.
5. **Usage by Content Type**: Here, you can examine which types of content items are being used more or less in the platform. You can view the data by subject, grade band, or product group. For every one of these categories, you can further select the individual subjects, grade bands, and product groups. You can also see an overall comparison of content type usage across the individual categories in a heatmap, and the actual counts in a table.
6. **Leaderboard**: The leaderboard section lets you discover your power users. Here, you can see which school districts are your top users. You can also click on any district in the leaderboard table and look at their usage over time (new and returning users over time and evetns per user over time).

## Steps to Run the Toolkits
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

### Step 4: Run the toolkits
To run any toolkit, you need to first prepare the RData files required for the toolkit. Once you have them in place in the toolkit folder, just run the `app.R` file in the individual toolkit folder.

## Data Science Services by Playpower Labs

Playpower Labs is one of the world's leading Educational Data Science consulting company. Our award winning team has worked with many different types of educational data. Examples include event data, assessment data, user behavior data, web analytics data, license and entitlement data, roster data, eText data, item response data, time series data, panel data, hierarchical data, skill and standard data, assignment data, knowledge structure data, school demographic data, and more.

If you need any help with the toolkits, please contact us at `nirmal@playpowerlabs.com`