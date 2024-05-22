
export class Promise<T> {}

/**
 * From T, pick a set of properties whose keys are in the union K
 */
export type Pick<T, K extends keyof T> = {
    [P in K]: T[P];
};

/**
 * Construct a type with a set of properties K of type T
 */
export type Record<K extends keyof any, T> = {
    [P in K]: T;
};