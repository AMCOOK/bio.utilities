--------------------------------------------------------
--  DDL for Table SDINF
--------------------------------------------------------

  CREATE TABLE "GROUNDFISH"."SDINF" 
   (	"DATASOURCE" VARCHAR2(3 BYTE), 
	"MISSION" VARCHAR2(15 BYTE), 
	"SETNO" NUMBER(3,0), 
	"SDATE" DATE, 
	"STIME" NUMBER(4,0), 
	"SLAT" NUMBER, 
	"SLONG" NUMBER, 
	"STRAT" VARCHAR2(3 BYTE), 
	"NAFO" VARCHAR2(10 BYTE), 
	"BOTTOM_TEMPERATURE" NUMBER(5,2), 
	"DEPTH" NUMBER(4,0), 
	"STATUS_FLAG" NUMBER
   ) PCTFREE 10 PCTUSED 40 INITRANS 1 MAXTRANS 255 NOCOMPRESS LOGGING
  STORAGE(INITIAL 1064960 NEXT 65536 MINEXTENTS 1 MAXEXTENTS 2147483645
  PCTINCREASE 0 FREELISTS 1 FREELIST GROUPS 1 BUFFER_POOL DEFAULT)
  TABLESPACE "MFD_GROUNDFISH" ;
 

   COMMENT ON COLUMN "GROUNDFISH"."SDINF"."DATASOURCE" IS 'Trip type code';
 
   COMMENT ON COLUMN "GROUNDFISH"."SDINF"."MISSION" IS 'Trip Id';
 
   COMMENT ON COLUMN "GROUNDFISH"."SDINF"."SETNO" IS 'Set Number';
 
   COMMENT ON COLUMN "GROUNDFISH"."SDINF"."SDATE" IS 'Set date';
 
   COMMENT ON COLUMN "GROUNDFISH"."SDINF"."STIME" IS 'Set time (24hr)';
 
   COMMENT ON COLUMN "GROUNDFISH"."SDINF"."SLAT" IS 'Set latitude (DDMM.MM)';
 
   COMMENT ON COLUMN "GROUNDFISH"."SDINF"."SLONG" IS 'Set longitude (DDMM.MM)';
 
   COMMENT ON COLUMN "GROUNDFISH"."SDINF"."STRAT" IS 'Stratum';
 
   COMMENT ON COLUMN "GROUNDFISH"."SDINF"."NAFO" IS 'NAFO division';
 
   COMMENT ON COLUMN "GROUNDFISH"."SDINF"."BOTTOM_TEMPERATURE" IS 'Water temperature in degrees Celsius';
 
   COMMENT ON COLUMN "GROUNDFISH"."SDINF"."DEPTH" IS 'bottom depth';
 
   COMMENT ON COLUMN "GROUNDFISH"."SDINF"."STATUS_FLAG" IS 'row status';
 
   COMMENT ON TABLE "GROUNDFISH"."SDINF"  IS 'Stomach database trip information ';
  GRANT SELECT ON "GROUNDFISH"."SDINF" TO "MFLIB" WITH GRANT OPTION;
 
  GRANT DELETE ON "GROUNDFISH"."SDINF" TO "BMACE";
 
  GRANT INSERT ON "GROUNDFISH"."SDINF" TO "BMACE";
 
  GRANT SELECT ON "GROUNDFISH"."SDINF" TO "BMACE";
 
  GRANT UPDATE ON "GROUNDFISH"."SDINF" TO "BMACE";
 
  GRANT SELECT ON "GROUNDFISH"."SDINF" TO "HARRISLE" WITH GRANT OPTION;
 
  GRANT SELECT ON "GROUNDFISH"."SDINF" TO "ABUNDY";
 
  GRANT SELECT ON "GROUNDFISH"."SDINF" TO "VDC";
 
  GRANT SELECT ON "GROUNDFISH"."SDINF" TO "VDC_DEV";
 
  GRANT SELECT ON "GROUNDFISH"."SDINF" TO "RICARDD";
 
  GRANT SELECT ON "GROUNDFISH"."SDINF" TO "HUBLEYB";
 
  GRANT SELECT ON "GROUNDFISH"."SDINF" TO "MACDONALDD";
 
  GRANT SELECT ON "GROUNDFISH"."SDINF" TO "GREYSONP";
