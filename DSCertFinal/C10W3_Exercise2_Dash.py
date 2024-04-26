# Import required libraries
import pandas as pd
import dash
from dash import html, dcc
from dash.dependencies import Input, Output
import plotly.express as px

# Read the airline data into pandas dataframe
spacex_df = pd.read_csv("spacex_launch_dash.csv")
max_payload = spacex_df['Payload Mass (kg)'].max()
min_payload = spacex_df['Payload Mass (kg)'].min()

launchSiteOpt = [ {'label': site, 'value':site} for site in spacex_df['Launch Site'].unique().tolist() ]
launchSiteOpt = [{'label': 'All Sites', 'value': 'ALL'}] + launchSiteOpt


# Create a dash application
app = dash.Dash(__name__)

# Create an app layout
app.layout = html.Div(
    children=[
        html.H1('SpaceX Launch Records Dashboard',
            style={'textAlign': 'center', 'color': '#503D36', 'font-size': 40}
        ),
        # TASK 1: Add a dropdown list to enable Launch Site selection
        # The default select value is for ALL sites
        # dcc.Dropdown(id='site-dropdown',...)
        html.Br(),
        #html.Label("Select Statistics:"),
        dcc.Dropdown(
            id = 'site-dropdown',
            options = launchSiteOpt,
            placeholder = 'Select a Launch Site here',
            searchable = True,
            style={'width':'80%', 'padding':'3px', 'font-size':'20px', 'textAlign':'center'}
        ),

        # TASK 2: Add a pie chart to show the total successful launches count for all sites
        # If a specific launch site was selected, show the Success vs. Failed counts for the site
        html.Div(dcc.Graph(id='success-pie-chart')),
        html.Br(),

        html.P("Payload range (Kg):"),
        # TASK 3: Add a slider to select payload range
        dcc.RangeSlider(
            id='payload-slider',
            min=0, max=10000, step=1000,
            #marks={0: '0', 100: '100'}, #unnecessary as min/max/step was set
            value=[min_payload, max_payload]
        ),

        # TASK 4: Add a scatter chart to show the correlation between payload and launch success
        html.Div(dcc.Graph(id='success-payload-scatter-chart')),
    ]
)

# TASK 2:
# Add a callback function for `site-dropdown` as input, `success-pie-chart` as output
# Function decorator to specify function input and output
@app.callback(
    Output(component_id='success-pie-chart', component_property='figure'),
    Input(component_id='site-dropdown', component_property='value')
)
def get_pie_chart(entered_site):
    if entered_site == 'ALL':
        fig = px.pie(
            spacex_df,
            values='class', 
            names='Launch Site', 
            title='Total Landing Outcomes from Each Launch Site'
        )
        return fig
    else:
        # return the outcomes piechart for a selected site
        filtered_df = spacex_df[ spacex_df['Launch Site']==entered_site ]['class']
        fig = px.pie(
            values=filtered_df.map(lambda x: 1),
            names=filtered_df,
            title=f'Landing Outcomes for the Launch Site:{entered_site}',
        )
        return fig


# TASK 4:
# Add a callback function for `site-dropdown` and `payload-slider` as inputs, `success-payload-scatter-chart` as output
@app.callback(
    Output(component_id='success-payload-scatter-chart', component_property='figure'),
    [Input(component_id='site-dropdown', component_property='value'), 
    Input(component_id="payload-slider", component_property="value")]
)
def get_scatter_chart(entered_site, entered_payloadM):
    if entered_site=="ALL":
        fig = px.scatter(
            spacex_df,
            x="Payload Mass (kg)", 
            y="class", 
            color="Booster Version Category", 
            title='Landing Outcomes vs. Payload Mass (kg)', 
        )
        return fig
    else:
        fig = px.scatter(
            spacex_df[ spacex_df['Launch Site']==entered_site ],
            x="Payload Mass (kg)", 
            y="class", 
            color="Booster Version Category", 
            title='Landing Outcomes vs. Payload Mass (kg)', 
        )
        return fig


# Run the app
if __name__ == '__main__':
    app.run_server()
