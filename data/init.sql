CREATE TABLE movies (
  title_id VARCHAR PRIMARY KEY,
  title VARCHAR NOT NULL,
  original_title VARCHAR NOT NULL,
  year INT,
  date_published DATE,
  genre VARCHAR,
  duration_in_mins BIGINT,
  country VARCHAR,
  language VARCHAR,
  director VARCHAR,
  writer VARCHAR,
  production_company VARCHAR,
  actors VARCHAR,
  description VARCHAR,
  avg_vote DOUBLE PRECISION,
  votes INT,
  budget JSONB,
  usa_gross_income VARCHAR,
  worlwide_gross_income VARCHAR,
  metascore DOUBLE PRECISION,
  reviews_from_users DOUBLE PRECISION,
  reviews_from_critics DOUBLE PRECISION
);

CREATE TABLE casting (
  name_id VARCHAR PRIMARY KEY,
  name VARCHAR NOT NULL,
  birth_name VARCHAR,
  height_in_cm INT,
  bio VARCHAR,
  birth_details VARCHAR,
  place_of_birth VARCHAR,
  death_details VARCHAR,
  date_of_death DATE
);

CREATE TABLE ratings (
  title_id VARCHAR NOT NULL,
  weighted_average DOUBLE PRECISION,
  total_votes INT,
  mean_vote DOUBLE PRECISION,
  median_vote DOUBLE PRECISION,
  CONSTRAINT title_id_fkey FOREIGN KEY (title_id)
    REFERENCES movies (title_id) MATCH SIMPLE
    ON UPDATE NO ACTION ON DELETE NO ACTION
);

CREATE TABLE principals (
  title_id VARCHAR NOT NULL,
  name_id VARCHAR NOT NULL,
  ordering INT,
  category VARCHAR,
  job VARCHAR,
  characters VARCHAR,
  CONSTRAINT title_id_fkey FOREIGN KEY (title_id)
    REFERENCES movies (title_id) MATCH SIMPLE
    ON UPDATE NO ACTION ON DELETE NO ACTION,
  CONSTRAINT name_id_fkey FOREIGN KEY (name_id)
    REFERENCES casting (name_id) MATCH SIMPLE
    ON UPDATE NO ACTION ON DELETE NO ACTION
);
