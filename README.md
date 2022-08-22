<h1>Solar Cell Research Application</h1>

<h2>Table of Contents</h2>
<ol>
    <li><a href="#overview">Overview</a></li>
    <li><a href="#access">Accessing The Application</a></li>
    <li><a href="#features">Notable Features</a></li>
    <ol>
        <li><a href="#heatmaps-demo">Visualizing Luminescence and Wavelength Values By Position</a></li>
        <li><a href="#table-1-demo">From Heatmaps to Table</a></li>
        <li><a href="#hist-demo">Distribution of Local/Absolute Max Luminescence</a></li>
        <ol>
            <li><a href="#hist-bin-demo">Bin Settings</a></li>
            <li><a href="#hist-titles-axis-demo">Title/Axis Settings</a></li>
        </ol>
        <li><a href="#spline-demo">Visualizing Luminescence vs. Wavelength of All Coordinates in One Plot</a></li>
    </ol>
    <li><a href="#packages">Packages Used</a></li>
    <li><a href="#publication">Publication</a></li>
</ol>
<hr>
<h2 id="overview">Overview</h2>
<p>The Solar Cell Research Application is a web application that is a product of the collaboration between both the Mathematics Department and the Engineering Technology Department at Texas A&M University-Central Texas. It was developed using R, specifically the Shiny package. The purpose of this interactive application is to enhance the Engineering Technology Department's research revolving around solar cells using statistics. It visualizes the characterization of the luminescence of solar cells, the distribution of the wavelengths associated with the luminescence, the relationship between luminescence and wavelengths and more.</p>
<hr>

<h2 id="access">Accessing The Application</h2>
<a href="https://sulley.shinyapps.io/Solar_Cell_Research_App/?_ga=2.51264047.727473287.1661166049-133858401.1636509877" target="_blank">Click here to access the Solar Cell Research Application</a>
<br />
<br />
<p>The program expects a cathodoluminescence dataset, either in TSV or CSV format, that is structured a certain way to be uploaded (refer to the “Welcome” page in the tab “File Format for Upload” for more information). When the application loads up to the “Welcome” page, for example, select “Comma” for the separator which is underneath the file upload widget. Then, download and upload the dataset <a href="./Example-Datasets/Allspectra S-8 20K CSV Format.csv">Allspectra S-8 20K CSV Format</a> to the program.</p>
<hr>

<h2 id="features">Notable Features</h2>
<!--Heatmaps Demonstration-->
<h3 id="heatmaps-demo">Visualizing Luminescence and Wavelength Values By Position</h3>
<p>Typically, an uploaded dataset is assumed by the program to consist of coordinate pairs, wavelengths, and luminescence values. To simply put, the relationship between luminescence, also referred to as intensity, and wavelength can vary by location. The program determines a smoothing spline that best fits the given points and then records the spline's absolute extremum. After storing said extremum for each coordinate pair in a table, the program determines which absolute max value, or absolute max intensity, is the greatest one. This value is called the <i><strong id="dataset-int">dataset intensity</strong></i>. The absolute max intensities are transformed by dividing each one by the dataset intensity making the possible values between 0 and 1; therefore, normalizing them (in the program, this normalization method is called <a href="#global-max"><strong>Global Max</strong></a>). The normalized values are added to the table, which is called “Table 1” in the program and can be viewed.</p>
<p>After Table 1 has been generated, the program utilizes it to form a heatmap visualizing the magnitude of the normalized absolute max intensities based on position. It's interactive and enables you to view the normalized value along with the coordinates.</p>

https://user-images.githubusercontent.com/66404238/145750544-55ca5c33-34da-4b45-ac1b-f7bbac542f69.MP4

<br />
<p>The wavelengths that correspond to the absolute max intensities are called peak wavelengths. To convey an idea of what wavelengths are associated with the absolute max intensities, like the previous heatmap, the program generates another one displaying the magnitude of the peak wavelengths based on position. It has the same capabilities as the other heatmap; however, you can specify the color gradient limits. Any tile with a peak wavelength that is out of the gradient's range is colored red/black if the peak wavelength is above/lower than the upper/lower limits, respectively. To apply the new values, click “Enter”; otherwise, in case the values need be reverted to the original ones, click “Reset”.</p>

https://user-images.githubusercontent.com/66404238/145750650-58e34508-724a-481e-99e2-443c2a545703.MP4

<br />


<!--Table 1 Demonstration-->
<h3 id="table-1-demo">From Heatmaps to Table</h3>
<p>You can get a glimpse of Table 1, which is in the “Dataset Tables” tab. In fact, it's interactive and allows sorting based on the column selected. Just click on the column name and the values will ascend/descend.</p>

https://user-images.githubusercontent.com/66404238/145750735-0b8c6686-9675-4b5a-aebc-256e18f318a5.MP4

<br />

<!--Local/Absolute Max Histogram Demonstration-->
<h3 id="hist-demo">Distribution of Local/Absolute Max Luminescence</h3>
<p>Another piece of information that is recorded by the program from the smoothing splines is the local extrema. As a result, the program visualizes the distributions of both the local and absolute max intensities in one plot. The histogram representing the distribution of the local max intensities is green, while the one for absolute max intensities is blue. Like the previous plots, this is interactive: Click on a bin and underneath you'll be presented with the bin limits and more information for either one or both histograms.</p>

https://user-images.githubusercontent.com/66404238/145750806-f15daa53-2f22-4c99-a9a6-4d06a79261c0.MP4

<br />

<!--Bin Settings Demonstration-->
<h4 id="hist-bin-demo">Bin Settings</h4>
<p>You have control over the number of bins for each histogram and whether you want to view one or both histograms. Also, the width of each bin, for both histograms, is shown underneath the sliders.</p>

https://user-images.githubusercontent.com/66404238/145750915-f6d24898-67f1-47c5-9521-cf2618141c2c.MP4

<br />

https://user-images.githubusercontent.com/66404238/145750984-c2421227-eeea-4010-83cc-09601d73a877.MP4


<!--Title/Axis Settings Demonstration-->
<h4 id="hist-titles-axis-demo">Title/Axis Settings</h4>
<p>You can change the histogram's default labels for the horizontal/vertical axis as well as the plot's title. Additionally, the horizontal axis can be further modified through the minimum, maximum, and increment inputs. Prefer the original values? Just click “Reset”. Otherwise, the new settings are applied by pressing “Apply Title/Axis Settings”.</p>

https://user-images.githubusercontent.com/66404238/145751048-c9d5d249-9e36-439b-b16d-1f76dee5d4f5.MP4

<!--Smoothing Spline Demonstration-->
<h3 id="spline-demo">Visualizing Luminescence vs. Wavelength of All Coordinates in One Plot</h3>
<p>Earlier it was mentioned that for each coordinate pair, the program finds the smoothing spline that best fits the given data points. To make use of it, the program draws out all the splines in one plot. This plot has three settings for the Normalization Method:</p>
<ul>
    <li><strong id="global-max">Global Max</strong> - the intensities are normalized by dividing each one by the <a href="#dataset-int">dataset intensity</a>.</li>
    <li><strong>Individual Spectra Max (Ind. Spectra Max)</strong> - the intensities are normalized by dividing each one by their respective location's absolute max intensity.</li>
    <li><strong>None</strong> - no normalization is applied to the intensities.</li>
</ul>

https://user-images.githubusercontent.com/66404238/145751107-6a0b2b5d-8b65-4ae7-acd2-95a834fd757b.MP4

<br />
<p>An additional feature of this plot is that hovering near a line with the cursor displays the row number the smooth curve represents.</p>

https://user-images.githubusercontent.com/66404238/145751187-5ca3616e-7e3e-46a8-aad8-a1125e3d3c60.MP4

<hr>

<h2 id="packages">Packages Used</h2>
<table>
    <thead>
        <tr>
            <td><strong>Package Name</strong></td>
            <td><strong>Version</strong></td>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>shiny</td>
            <td>1.7.1</td>
        </tr>
        <tr>
            <td>ggplot2</td>
            <td>3.3.5</td>
        </tr>
        <tr>
            <td>DT</td>
            <td>0.19</td>
        </tr>
        <tr>
            <td>shinyWidgets</td>
            <td>0.6.2</td>
        </tr>
        <tr>
            <td>shinycssloaders</td>
            <td>1.0.0</td>
        </tr>
        <tr>
            <td>SplinesUtils</td>
            <td>0.2</td>
        </tr>
        <tr>
            <td>dplyr</td>
            <td>1.0.7</td>
        </tr>
        <tr>
            <td>data.table</td>
            <td>1.14.2</td>
        </tr>
        <tr>
            <td>shinydashboard</td>
            <td>0.7.2</td>
        </tr>
        <tr>
            <td>readr</td>
            <td>2.0.2</td>
        </tr>
        <tr>
            <td>hrbrthemes</td>
            <td>0.8.0</td>
        </tr>
    </tbody>
</table>
<hr>

<h2 id="publication">Publication</h2>
<p>A previous version of the application was presented at the 2020 International Conference of Advanced Research in Applied Science, Engineering and Technology (ICARASET '20). The following is the article published:</p>
<p><a href="http://ibii-us.org/Journals/JESD/V4N1/Publish/N4V1_3.pdf">Analyze and visualize cathodoluminescence data obtained from images of a photovoltaic cell using the R language</a></p>
