/*ONLINE DATABASE FOR THE PLUGUP SERVICE*/

CREATE SCHEMA jobservice AUTHORIZATION plugupservice;

CREATE TABLE jobservice.client_connection (
	cnpj_cpf varchar(14) NOT NULL,
	additional_info json NULL,
	connection_info json NULL,
	id uuid DEFAULT gen_random_uuid() NOT NULL,
	CONSTRAINT client_connection_pk PRIMARY KEY (id),
	CONSTRAINT client_connection_unique UNIQUE (cnpj_cpf)
);

insert into jobservice.client_connection (cnpj_cpf, additional_info, connection_info)
values ('19503009000143', '{"name":"MORENA SHOW","plan":"PLANO MORENA SHOW", "blocked":false}', 
'{"srv":"db1.wl9.aprendaerp.com.br","user":"forupdev04","senha":"ForU9d3v*","database":"67c96776-7732-4eff-b668-492bc4bbb8b4","usuaid":"67c96776-7732-4eff-b668-492bc4bbb8b4","usuaemail":"morenashow@forupsolucoes.com.br"}');

insert into jobservice.client_connection (cnpj_cpf, additional_info, connection_info)
values ('34567753000103', '{"name":"TIJUCANO PRÉ-MOLDADOS","plan":"PLANO TIJUCANO", "blocked":false}', 
'{"srv":"db1.wl9.aprendaerp.com.br","user":"forupdev07","senha":"ForU9d3v*","database":"7c1305fc-13ff-4b0a-8c72-48185250f4a6","usuaid":"7c1305fc-13ff-4b0a-8c72-48185250f4a6","usuaemail":"tijucano@forupsolucoes.com.br"}');

insert into jobservice.client_connection (cnpj_cpf, additional_info, connection_info)
values ('54580848000166', '{"name":"CONVENIENCIA CENTRAL","plan":"PLANO CENTRAL", "blocked":false}', 
'{"srv":"db1.wl10.aprendaerp.com.br","user":"forupdev08","senha":"ForU9d3v*","database":"7ff7e518-bafa-4156-a615-8c1f802dc445","usuaid":"7ff7e518-bafa-4156-a615-8c1f802dc445","usuaemail":"adm.central@forupsolucoes.com.br"}');

CREATE TABLE jobservice.job_config (
	id uuid DEFAULT gen_random_uuid() NOT NULL,
	client_id uuid not null references jobservice.client_connection(id),
	job_info json NOT NULL,
	job_params json NOT NULL,
	CONSTRAINT job_config_pk PRIMARY KEY (id)
);

insert into jobservice.job_config (client_id, job_info, job_params)
values ((select id from jobservice.client_connection where cnpj_cpf='19503009000143'),'{"job_name":"Imprimir Venda MOB","job_description":"Imprime a venda executada no aplicativo MOBI ERP"}',
'{"job_interval":30,"job_purge":false,"job_pull_entity":[{"entity":"DtoVenda","fields":["all"],"master_id":"_id","children":[{"entity":"DtoVendaProduto","fields":["all"],"master_id":"VendaID"}]}],"job_wait_status":["OrigemVenda = ''PDV''","FoiConferido = false","Impresso = false","Codigo >= 738"],"job_done_status":["FoiConferido = true","Impresso = true"]}');

CREATE TABLE jobservice.jobs_waiting (
	id uuid DEFAULT gen_random_uuid() NOT NULL,
	job_id uuid NOT NULL,
	job_waiting_from timestamp without time zone DEFAULT now() NOT NULL,
	job_data json NOT NULL,
	job_collected_at timestamp without time zone NULL,
	job_status varchar(1) DEFAULT 'W' NULL,
	CONSTRAINT jobs_waiting_pk PRIMARY KEY (id)
);


