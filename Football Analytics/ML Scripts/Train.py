import pymysql
import pandas as pd
from sklearn.preprocessing import RobustScaler, LabelEncoder
from sklearn.ensemble import AdaBoostClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn.metrics import accuracy_score, f1_score, recall_score, precision_score
from sklearn.model_selection import train_test_split, GridSearchCV
import pickle
import os
from sqlalchemy import create_engine

# Actualizar con la ruta absoluta de la carpeta donde se van a crear los paises
# Ejemplo: C:/releases/ML_Models/WinnerModel/
abs_path = 'C:/releases/ML_Models/WinnerModel/V13/'

# Business key of the bet label
bk_bet_label = '2'
algorithm = 'ADA3'

# Connection variables
db_user = 'powerbi'
db_password = 'powerbi'
db_database = 'uty46vowri'
db_host = '107.173.168.162'

# Queries
query_countries = '''select distinct ds_country from dwh_dim_league where mc_actual = 1'''
query_extract = '''select m.* from vw_model2_ml m
where ds_country = "{}" and bk_date_start >= 20170000
'''


def extract_countries(user, password, database, host, query):
    connection = pymysql.connect(user=user, password=password, database=database, host=host)
    df_countries = pd.read_sql(query, connection)
    connection.close()
    list_countries = df_countries['ds_country'].values.tolist()

    return list_countries


def extract_data(user, password, database, host, query):
    connection = pymysql.connect(user=user, password=password, database=database, host=host)
    df_data = pd.read_sql(query, connection)
    connection.close()

    return df_data


def calculate_result(row):
    if row['goal_diff'] > 0:
        return 'W'
    elif row['goal_diff'] < 0:
        return 'L'
    else:
        return 'T'


def transform_data(df1):
    df1.dropna(inplace=True)
    df1['perc_wins'] = df1['vl_wins_home'] / df1['vl_played_home']
    df1['perc_draws'] = df1['vl_draws_home'] / df1['vl_played_home']
    df1['perc_loses'] = df1['vl_loses_home'] / df1['vl_played_home']
    df1['diff_goles'] = df1['GOALS_TEAM_HOME'] - df1['goals_team_away']
    df1 = df1[['SK_FIXTURE', 'HOME_TEAM', 'AWAY_TEAM', 't1_wins_home', 't1_draws_home', 't1_loses_home',
               't1_goals_for_home', 't1_goals_against_home', 't1_goals_avg_for_home', 't1_goals_avg_against_home',
               't2_wins_away', 't2_draws_away', 't2_loses_away', 't2_goals_for_away', 't2_goals_against_away',
               't2_goals_avg_for_away', 't2_goals_avg_against_away', 'winning_percent_home', 'WINNING_PERCENT_AWAY',
               'winning_percent_draws', 'perc_wins', 'perc_draws', 'perc_loses', 'diff_goles']]
    df1 = df1.rename(columns={'SK_FIXTURE': 'sk_fixture', 'winning_percent_home': 'proba_win_home',
                              'WINNING_PERCENT_AWAY': 'proba_win_away', 'winning_percent_draws': 'proba_win_draw',
                              'perc_wins': '%_wins_home', 'perc_draws': '%_draws_home', 'perc_loses': '%_loses_home',
                              'diff_goles': 'goal_diff'})
    dff = df1[['sk_fixture', 't1_goals_avg_for_home', 't1_wins_home', 't2_goals_avg_for_away',
               't1_goals_avg_against_home', 't2_wins_away', 't2_goals_avg_against_away', 't1_loses_home',
               't2_loses_away', '%_loses_home', '%_wins_home', '%_draws_home', 'goal_diff']]
    dff['result'] = dff.apply(lambda row: calculate_result(row), axis=1)
    dff = dff.drop(['goal_diff'], axis=1)
    dff.dropna(inplace=True)
    df_stand = dff.drop(['sk_fixture'], axis=1)
    robust_scale = RobustScaler(copy=True, with_centering=True, with_scaling=True)
    df_feat = df_stand.drop(['result'], axis=1)
    robust_scaled_array = robust_scale.fit_transform(df_feat)
    df_scaled = pd.DataFrame(robust_scaled_array, columns=df_feat.columns)
    dff.reset_index(drop=True, inplace=True)
    df_scaled['result'] = dff['result']
    df_scaled['sk_fixture'] = dff['sk_fixture']
    lbl = LabelEncoder()
    lbl.fit(df_scaled['result'])
    df_scaled['result'] = lbl.transform(df_scaled['result'])

    return df_scaled, robust_scale, lbl


def train_model(df1):
    X = df1.drop(['result'], axis=1)
    Y = df1['result']
    X_train, X_test, y_train, y_test = train_test_split(X, Y, random_state=0)

    X_train = X_train.drop(['sk_fixture'], axis=1)
    X_test = X_test.drop(['sk_fixture'], axis=1)

    dt_class = DecisionTreeClassifier(max_depth=1)
    clf = AdaBoostClassifier(n_estimators=100, base_estimator=dt_class, learning_rate=0.9)
    clf.fit(X_train, y_train)
    y_predicted = clf.predict(X_test)
    accuracy = accuracy_score(y_test, y_predicted)
    recall = recall_score(y_test, y_predicted, average='weighted')
    precision = precision_score(y_test, y_predicted, average='weighted')
    roc_auc = f1_score(y_test, y_predicted, average='weighted')

    return clf, roc_auc, accuracy, recall, precision


def create_pickles(path, rb, lb, cl1, c):

    if not os.path.exists(path + c):
        os.makedirs(path + c)

    path_rb = path + c + '/robust_scale.p'
    path_lb = path + c + '/label_encode.p'
    path_cl = path + c + '/classifier.p'

    pickle.dump(rb, open(path_rb, 'wb'))
    pickle.dump(lb, open(path_lb, 'wb'))
    pickle.dump(cl1, open(path_cl, 'wb'))


def model_to_database(user, password, database, host, md_id, bet_label_id, c, ds_algorithm, vl_aucroc, vl_accuracy,
                      vl_recall, vl_precision):

    dict1 = {'model_id': md_id, 'bet_label_id': bet_label_id, 'country': c, 'ds_algorithm': ds_algorithm,
             'vl_aucroc': vl_aucroc, 'vl_accuracy': vl_accuracy, 'vl_recall': vl_recall, 'vl_precision': vl_precision}
    rows_list = [dict1]
    df_result = pd.DataFrame(rows_list)
    string_connect = 'mysql+pymysql://' + user + ':' + password + '@' + host + '/' + database
    sqlEngine = create_engine(string_connect)
    dbConnection = sqlEngine.connect()

    try:
        df_result.to_sql('ext_dim_ml_model', dbConnection, index=False, if_exists='append')
    except ValueError as vx:
        print(vx)
    except Exception as ex:
        print(ex)
    finally:
        dbConnection.close()


country_list = extract_countries(db_user, db_password, db_database, db_host, query_countries)

for country in country_list:
    print("Starts:", country)

    query_def = query_extract.format(country)
    df = extract_data(db_user, db_password, db_database, db_host, query_def)
    print("Data Extracted", df.shape)

    df, re, le = transform_data(df)
    print("Data Transformed", df.shape)

    cl, roc, acc, rec, prc = train_model(df)
    print("Data Trained")

    create_pickles(abs_path, re, le, cl, country)
    print("Pickles Created")

    model_id = bk_bet_label + '-' + country + '-' + algorithm
    model_to_database(db_user, db_password, db_database, db_host, model_id, bk_bet_label, country, algorithm,
                      roc, acc, rec, prc)
    print("Model Data Uploaded")
    print("Finishes:", country)
