import {Decl, Type, Loc} from "./schema";

export interface APIMeta {
  module_path: string;
  pkgs: Package[];
  svcs: Service[];
  version: string;
  decls: Decl[];
  auth_handler: AuthHandler;
}

export interface Service {
  name: string;
  rel_path: string;
  rpcs: RPC[];
  migrations: DBMigration[];
}

export interface Package {
  rel_path: string;
  name: string;
  doc: string;
  svc: string; // can be empty
  secrets: string[];
  rpc_calls: QualifiedName[];
}

export interface RPC {
  name: string;
  doc: string;
  access_type: "PRIVATE" | "PUBLIC" | "AUTH";
  rpc_calls: QualifiedName[];
  request_schema?: Type;
  response_schema?: Type;
  proto: "REGULAR" | "RAW";
  loc: Loc;
  path: Path;
  http_methods: string[];
}

export interface QualifiedName {
  pkg: string;
  name: string;
}

export interface DBMigration {
  filename: string;
  number: number;
  description: string;
  up: boolean;
}

export interface AuthHandler {
  name: string;
  doc: string;
  user_data?: Type;
  loc: Loc;
}

export interface Path {
  segments: PathSegment[];
}

export interface PathSegment {
  type: "LITERAL" | "PARAM" | "WILDCARD";
  value: string;
}
