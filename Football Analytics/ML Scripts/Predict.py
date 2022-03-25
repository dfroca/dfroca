import pymysql
import pandas as pd
import pickle
from sklearn.preprocessing import RobustScaler, LabelEncoder
from sklearn.ensemble import AdaBoostClassifier
from sklearn.tree import DecisionTreeClassifier
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
query_extract = '''select m.* from vw_model2_ml_predict m
left join dwh_dim_fixture f on f.sk_fixture = m.sk_fixture
left join dwh_dim_date d on f.bk_date_start = d.bk_date
left join dwh_dim_league l on l.bk_league = f.bk_league
where f.ds_status = 'Not Started'
AND (`d`.`DT_DATE` = (CURDATE() + INTERVAL 1 DAY))
and l.ds_country = "{}"
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


def load_pickles(path, c):

    path_rb = path + c + '/robust_scale.p'
    path_lb = path + c + '/label_encode.p'
    path_cl = path + c + '/classifier.p'

    # Cargar los scaler, label encoder y svm que se crearon en el entrenamiento
    robust_scale = pickle.load(open(path_rb, 'rb'))
    lbl = pickle.load(open(path_lb, 'rb'))
    clf = pickle.load(open(path_cl, 'rb'))

    return robust_scale, lbl, clf


def transform_data(df1, rs):
    df1['perc_wins'] = df1['vl_wins_home'] / df1['vl_played_home']
    df1['perc_draws'] = df1['vl_draws_home'] / df1['vl_played_home']
    df1['perc_loses'] = df1['vl_loses_home'] / df1['vl_played_home']
    df1 = df1.fillna(0)
    df1 = df1[['SK_FIXTURE', 'HOME_TEAM', 'AWAY_TEAM', 't1_wins_home', 't1_draws_home', 't1_loses_home',
               't1_goals_for_home', 't1_goals_against_home', 't1_goals_avg_for_home', 't1_goals_avg_against_home',
               't2_wins_away', 't2_draws_away', 't2_loses_away', 't2_goals_for_away', 't2_goals_against_away',
               't2_goals_avg_for_away', 't2_goals_avg_against_away', 'perc_wins', 'perc_draws', 'perc_loses']]
    df1 = df1.rename(columns={'SK_FIXTURE': 'sk_fixture', 'perc_wins': '%_wins_home',
                              'perc_draws': '%_draws_home', 'perc_loses': '%_loses_home'})
    # Viendo la correalación y la regresión de los features vs el target se seleccionan los siguientes features:
    df_features = df1[['sk_fixture', 't1_goals_avg_for_home', 't1_wins_home', 't2_goals_avg_for_away',
                       't1_goals_avg_against_home', 't2_wins_away', 't2_goals_avg_against_away', 't1_loses_home',
                       't2_loses_away', '%_loses_home', '%_wins_home', '%_draws_home']]
    df_stand = df_features.drop(['sk_fixture'], axis=1)
    robust_scaled_array = rs.transform(df_stand)
    df_scaled = pd.DataFrame(robust_scaled_array, columns=df_stand.columns)
    df_scaled['sk_fixture'] = df_features['sk_fixture']

    return df_scaled


def predict(df1, clf, lbl, bet_label, ds_algorithm, c):
    sk = df1['sk_fixture']
    df_scaled = df1.drop('sk_fixture', axis=1)
    probabilities = clf.predict_proba(df_scaled)
    predictions = clf.predict(df_scaled)
    predictions = lbl.inverse_transform(predictions)
    df_probabilities = pd.DataFrame(probabilities, columns=lbl.inverse_transform(clf.classes_))
    df_probabilities['prediction'] = predictions
    df_fixture = pd.DataFrame(sk, columns=['sk_fixture'])
    df_fixture.reset_index(drop=True, inplace=True)
    df_probabilities['sk_fixture'] = df_fixture['sk_fixture']
    cols = ['sk_fixture', 'L', 'T', 'W', 'prediction']
    df_final = df_probabilities[cols]
    df_final['bk_model'] = bet_label + '-' + c + '-' + ds_algorithm

    return df_final


def data_to_database(user, password, database, host, df1):

    string_connect = 'mysql+pymysql://' + user + ':' + password + '@' + host + '/' + database
    sqlEngine = create_engine(string_connect)
    dbConnection = sqlEngine.connect()

    try:
        df1.to_sql('ext_fact_winner_model', dbConnection, index=False, if_exists='append')
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
    if df.shape[0] > 0:
        re, le, cl = load_pickles(abs_path, country)
        print("Pickles Loaded")
        df = transform_data(df, re)
        print("Data Transformed")
        df = predict(df, cl, le, bk_bet_label, algorithm, country)
        print("Data Predicted")
        data_to_database(db_user, db_password, db_database, db_host, df)
        print("Data Loaded to DB")

    print("Finishes:", country)
