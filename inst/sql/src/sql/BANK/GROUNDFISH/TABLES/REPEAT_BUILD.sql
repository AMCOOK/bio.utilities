--------------------------------------------------------
--  DDL for Table REPEAT_BUILD
--------------------------------------------------------

  CREATE TABLE "GROUNDFISH"."REPEAT_BUILD" 
   (	"SERIES" VARCHAR2(15 BYTE), 
	"REGION" VARCHAR2(5 BYTE), 
	"MISSION" VARCHAR2(15 BYTE), 
	"SETNO" VARCHAR2(4 BYTE), 
	"STATION" VARCHAR2(4 BYTE), 
	"XTYPE" VARCHAR2(1 BYTE), 
	"SEP_NAUT_MI" NUMBER, 
	"SEP_DAYS" NUMBER
   ) PCTFREE 10 PCTUSED 40 INITRANS 1 MAXTRANS 255 NOCOMPRESS LOGGING
  STORAGE(INITIAL 163840 NEXT 65536 MINEXTENTS 1 MAXEXTENTS 2147483645
  PCTINCREASE 0 FREELISTS 1 FREELIST GROUPS 1 BUFFER_POOL DEFAULT)
  TABLESPACE "MFD_GROUNDFISH" ;
 

   COMMENT ON COLUMN "GROUNDFISH"."REPEAT_BUILD"."SERIES" IS 'Series';
 
   COMMENT ON COLUMN "GROUNDFISH"."REPEAT_BUILD"."REGION" IS 'Region';
 
   COMMENT ON COLUMN "GROUNDFISH"."REPEAT_BUILD"."MISSION" IS 'vessel or Survey ID';
 
   COMMENT ON COLUMN "GROUNDFISH"."REPEAT_BUILD"."SETNO" IS 'Set number';
 
   COMMENT ON COLUMN "GROUNDFISH"."REPEAT_BUILD"."STATION" IS 'Repeats Station (lowest setno)';
 
   COMMENT ON COLUMN "GROUNDFISH"."REPEAT_BUILD"."XTYPE" IS 'experiment type';
 
   COMMENT ON COLUMN "GROUNDFISH"."REPEAT_BUILD"."SEP_NAUT_MI" IS 'Separation in nautical miles';
 
   COMMENT ON COLUMN "GROUNDFISH"."REPEAT_BUILD"."SEP_DAYS" IS 'Separation in days';
 
   COMMENT ON TABLE "GROUNDFISH"."REPEAT_BUILD"  IS 'Repeated Sets Selection Parameters ';
  GRANT SELECT ON "GROUNDFISH"."REPEAT_BUILD" TO "MFLIB" WITH GRANT OPTION;
 
  GRANT SELECT ON "GROUNDFISH"."REPEAT_BUILD" TO "VDC";
 
  GRANT SELECT ON "GROUNDFISH"."REPEAT_BUILD" TO "VDC_DEV";
 
  GRANT SELECT ON "GROUNDFISH"."REPEAT_BUILD" TO "RICARDD";
 
  GRANT SELECT ON "GROUNDFISH"."REPEAT_BUILD" TO "HUBLEYB";
 
  GRANT SELECT ON "GROUNDFISH"."REPEAT_BUILD" TO "GREYSONP";
