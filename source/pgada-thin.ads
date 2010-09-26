------------------------------------------------------------------------------
--                                                                          --
--                           P G A D A . T H I N                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--  Copyright (c) coreland 2009                                             --
--  Copyright (c) Samuel Tardieu 2000                                       --
--  All rights reserved.                                                    --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions      --
--  are met:                                                                --
--  1. Redistributions of source code must retain the above copyright       --
--     notice, this list of conditions and the following disclaimer.        --
--  2. Redistributions in binary form must reproduce the above copyright    --
--     notice, this list of conditions and the following disclaimer in      --
--     the documentation and/or other materials provided with the           --
--     distribution.                                                        --
--  3. Neither the name of Samuel Tardieu nor the names of its contributors --
--     may be used to endorse or promote products derived from this         --
--     software without specific prior written permission.                  --
--                                                                          --
--  THIS SOFTWARE IS PROVIDED BY SAMUEL TARDIEU AND CONTRIBUTORS ``AS       --
--  IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT          --
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       --
--  FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL SAMUEL      --
--  TARDIEU OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,             --
--  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES                --
--  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR      --
--  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)      --
--  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN               --
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR            --
--  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,          --
--  EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                      --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces.C.Strings;
with Interfaces.C;

with System;

package PGAda.Thin is
   pragma Preelaborate;

   type Conn_Status is (CONNECTION_OK, CONNECTION_BAD);
   for Conn_Status'Size use IC.int'Size;
   pragma Convention (C, Conn_Status);

   type Exec_Status is
     (PGRES_EMPTY_QUERY,
      PGRES_COMMAND_OK,
      PGRES_TUPLES_OK,
      PGRES_COPY_OUT,
      PGRES_COPY_IN,
      PGRES_BAD_RESPONSE,
      PGRES_NONFATAL_ERROR,
      PGRES_FATAL_ERROR);

   for Exec_Status'Size use IC.int'Size;
   pragma Convention (C, Exec_Status);

   type PG_Conn is null record;
   type PG_Conn_Access is access PG_Conn;
   pragma Convention (C, PG_Conn_Access);

   type PG_Result is null record;
   type PG_Result_Access is access PG_Result;
   pragma Convention (C, PG_Result_Access);

   type Oid is new IC.unsigned;

   type Error_Field is
     (PG_DIAG_SQLSTATE,
      PG_DIAG_MESSAGE_DETAIL,
      PG_DIAG_SOURCE_FILE,
      PG_DIAG_MESSAGE_HINT,
      PG_DIAG_SOURCE_LINE,
      PG_DIAG_MESSAGE_PRIMARY,
      PG_DIAG_STATEMENT_POSITION,
      PG_DIAG_SOURCE_FUNCTION,
      PG_DIAG_SEVERITY,
      PG_DIAG_CONTEXT,
      PG_DIAG_INTERNAL_POSITION,
      PG_DIAG_INTERNAL_QUERY);
   for Error_Field use
     (PG_DIAG_SQLSTATE           => 67,
      PG_DIAG_MESSAGE_DETAIL     => 68,
      PG_DIAG_SOURCE_FILE        => 70,
      PG_DIAG_MESSAGE_HINT       => 72,
      PG_DIAG_SOURCE_LINE        => 76,
      PG_DIAG_MESSAGE_PRIMARY    => 77,
      PG_DIAG_STATEMENT_POSITION => 80,
      PG_DIAG_SOURCE_FUNCTION    => 82,
      PG_DIAG_SEVERITY           => 83,
      PG_DIAG_CONTEXT            => 87,
      PG_DIAG_INTERNAL_POSITION  => 112,
      PG_DIAG_INTERNAL_QUERY     => 113);
   for Error_Field'Size use IC.int'Size;

   function PQ_Connect_DB (Connection_Info : ICS.Chars_Ptr) return PG_Conn_Access;
   pragma Import (C, PQ_Connect_DB, "PQconnectdb");

   function PQ_Set_Db_Login
     (PG_Host    : ICS.chars_ptr;
      PG_Port    : ICS.chars_ptr;
      PG_Options : ICS.chars_ptr;
      PG_TTY     : ICS.chars_ptr;
      Db_Name    : ICS.chars_ptr;
      Login      : ICS.chars_ptr;
      Password   : ICS.chars_ptr)
   return PG_Conn_Access;
   pragma Import (C, PQ_Set_Db_Login, "PQsetdbLogin");

   function PQ_Db (Conn : PG_Conn_Access) return ICS.chars_ptr;
   pragma Import (C, PQ_Db, "PQdb");

   function PQ_Host (Conn : PG_Conn_Access) return ICS.chars_ptr;
   pragma Import (C, PQ_Host, "PQhost");

   function PQ_Port (Conn : PG_Conn_Access) return ICS.chars_ptr;
   pragma Import (C, PQ_Port, "PQport");

   function PQ_Options (Conn : PG_Conn_Access) return ICS.chars_ptr;
   pragma Import (C, PQ_Options, "PQoptions");

   function PQ_Status (Conn : PG_Conn_Access) return Conn_Status;
   pragma Import (C, PQ_Status, "PQstatus");

   function PQ_Error_Message (Conn : PG_Conn_Access) return ICS.chars_ptr;
   pragma Import (C, PQ_Error_Message, "PQerrorMessage");

   procedure PQ_Finish (Conn : in PG_Conn_Access);
   pragma Import (C, PQ_Finish, "PQfinish");

   procedure PQ_Reset (Conn : in PG_Conn_Access);
   pragma Import (C, PQ_Reset, "PQreset");

   function PQ_Exec
     (Conn  : PG_Conn_Access;
      Query : ICS.chars_ptr)
   return PG_Result_Access;
   pragma Import (C, PQ_Exec, "PQexec");

   function PQ_Result_Status (Res : PG_Result_Access) return Exec_Status;
   pragma Import (C, PQ_Result_Status, "PQresultStatus");

   function PQ_N_Tuples (Res : PG_Result_Access) return IC.int;
   pragma Import (C, PQ_N_Tuples, "PQntuples");

   function PQ_N_Fields (Res : PG_Result_Access) return IC.int;
   pragma Import (C, PQ_N_Fields, "PQnfields");

   function PQ_F_Name
     (Res         : PG_Result_Access;
      Field_Index : IC.int)
   return ICS.chars_ptr;
   pragma Import (C, PQ_F_Name, "PQfname");

   function PQ_F_Number
     (Res         : PG_Result_Access;
      Field_Index : ICS.chars_ptr)
   return IC.int;
   pragma Import (C, PQ_F_Number, "PQfnumber");

   function PQ_F_Type
     (Res         : PG_Result_Access;
      Field_Index : IC.int)
   return Oid;
   pragma Import (C, PQ_F_Type, "PQftype");

   function PQ_Get_Value
     (Res       : PG_Result_Access;
      Tup_Num   : IC.int;
      Field_Num : IC.int)
   return ICS.chars_ptr;
   pragma Import (C, PQ_Get_Value, "PQgetvalue");

   function PQ_Get_Length
     (Res       : PG_Result_Access;
      Tup_Num   : IC.int;
      Field_Num : IC.int)
   return IC.int;
   pragma Import (C, PQ_Get_Length, "PQgetlength");

   function PQ_Get_Is_Null
     (Res       : PG_Result_Access;
      Tup_Num   : IC.int;
      Field_Num : IC.int)
   return IC.int;
   pragma Import (C, PQ_Get_Is_Null, "PQgetisnull");

   function PQ_Cmd_Tuples (Res : PG_Result_Access) return ICS.chars_ptr;
   pragma Import (C, PQ_Cmd_Tuples, "PQcmdTuples");

   function PQ_Cmd_Status (Res : PG_Result_Access) return ICS.chars_ptr;
   pragma Import (C, PQ_Cmd_Status, "PQcmdStatus");

   function PQ_Oid_Status (Res : PG_Result_Access) return ICS.chars_ptr;
   pragma Import (C, PQ_Oid_Status, "PQoidStatus");

   procedure PQ_Clear (Res : in PG_Result_Access);
   pragma Import (C, PQ_Clear, "PQclear");

   function PQ_Result_Error_Field
     (Res   : PG_Result_Access;
      Field : Error_Field)
   return ICS.chars_ptr;
   pragma Import (C, PQ_Result_Error_Field, "PQresultErrorField");

   function PQ_Prepare
     (Conn           : PG_Conn_Access;
      Statement_Name : ICS.chars_ptr;
      Query          : ICS.chars_ptr;
      Params         : IC.int;
      Param_Types    : System.Address)
   return PG_Result_Access;
   pragma Import (C, PQ_Prepare, "PQprepare");

   type Int_Access is access IC.Int;

   function PQ_Exec_Prepared
     (Conn           : PG_Conn_Access;
      Statement_Name : ICS.chars_ptr;
      Params         : IC.int;
      Param_Values   : System.Address;
      Param_Lengths  : Int_Access;
      Param_Formats  : Int_Access;
      Result_Format  : IC.int)
   return PG_Result_Access;
   pragma Import (C, PQ_Exec_Prepared, "PQexecPrepared");

end PGAda.Thin;
