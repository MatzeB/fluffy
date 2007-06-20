drop table if exists persons;
create table persons (
	ID int not null auto_increment unique,
	title VARCHAR(255),
	pre_name VARCHAR(255),
	middle_name VARCHAR(255),
	last_name VARCHAR(255),
	suffix VARCHAR(255),
	primary key(ID)
) type=InnoDB;

drop table if exists addresses;
create table addresses (
	ID int not null auto_increment unique,
	personID int not null references persons(ID),
	address TINYTEXT,
	postbox VARCHAR(255),
	city VARCHAR(255),
	zip_code VARCHAR(63),
	state VARCHAR(255),
	country VARCHAR(255),

	primary key(ID)
) type=InnoDB;

drop table if exists phone_numbers;
create table phone_numbers (
	ID int not null auto_increment unique,
	personID int not null references persons(ID),
	phone_number VARCHAR(255),

	primary key(ID)
) type=InnoDB;

