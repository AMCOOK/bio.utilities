--------------------------------------------------------
--  DDL for Table CFTIMESTAMPS
--------------------------------------------------------

  CREATE TABLE "GROUNDFISH"."CFTIMESTAMPS" 
   (	"ID" NUMBER(2,0), 
	"PROCEDURE" VARCHAR2(35 BYTE), 
	"TIMESTAMP" DATE
   ) PCTFREE 10 PCTUSED 40 INITRANS 1 MAXTRANS 255 NOCOMPRESS LOGGING
  STORAGE(INITIAL 106496 NEXT 65536 MINEXTENTS 1 MAXEXTENTS 2147483645
  PCTINCREASE 0 FREELISTS 1 FREELIST GROUPS 1 BUFFER_POOL DEFAULT)
  TABLESPACE "MFD_GROUNDFISH" ;
 

   COMMENT ON COLUMN "GROUNDFISH"."CFTIMESTAMPS"."ID" IS 'ID number';
 
   COMMENT ON COLUMN "GROUNDFISH"."CFTIMESTAMPS"."PROCEDURE" IS 'Procedure name as shown on the VDC';
 
   COMMENT ON COLUMN "GROUNDFISH"."CFTIMESTAMPS"."TIMESTAMP" IS 'Timestamp';
 
   COMMENT ON TABLE "GROUNDFISH"."CFTIMESTAMPS"  IS 'Table to store date/time stamp of last procedure execution';
  GRANT SELECT ON "GROUNDFISH"."CFTIMESTAMPS" TO "MFLIB";
 
  GRANT SELECT ON "GROUNDFISH"."CFTIMESTAMPS" TO "HURLEYP" WITH GRANT OPTION;
 
  GRANT SELECT ON "GROUNDFISH"."CFTIMESTAMPS" TO "VDC";
 
  GRANT SELECT ON "GROUNDFISH"."CFTIMESTAMPS" TO "VDC_DEV";
 
  GRANT SELECT ON "GROUNDFISH"."CFTIMESTAMPS" TO "RICARDD";
 
  GRANT SELECT ON "GROUNDFISH"."CFTIMESTAMPS" TO "HUBLEYB";
 
  GRANT SELECT ON "GROUNDFISH"."CFTIMESTAMPS" TO "CAPECHIDLEY" WITH GRANT OPTION;
 
  GRANT SELECT ON "GROUNDFISH"."CFTIMESTAMPS" TO "GREYSONP";
