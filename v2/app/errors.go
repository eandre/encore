package app

import (
	"encr.dev/pkg/errors"
)

const (
	serviceHelp = "For more information on services and how to define them, see https://encore.dev/docs/primitives/services-and-apis"
)

var (
	errRange = errors.Range(
		"app",
		"",
	)

	errServiceContainedWitinAnother = errRange.Newf(
		"Service contained within another service",
		"The service %s was found within the service %s. Encore does not allow services to be nested.",
		errors.WithDetails(serviceHelp),
	)

	errDuplicateServiceNames = errRange.Newf(
		"Duplicate service names",
		"Two services were found with the same name %q, services must have unique names.",
	)

	errNoServiceFound = errRange.Newf(
		"No service found",
		"No service was found for package %q.",
		errors.MarkAsInternalError(),
	)
)
