package resource

import (
	"encr.dev/v2/internal/pkginfo"
)

//go:generate stringer -type=Kind -output=resource_string.go

type Kind int

const (
	Unknown Kind = iota

	// Infrastructure SDK Resources
	PubSubTopic
	PubSubSubscription
	SQLDatabase
	Metric
	CronJob
	CacheCluster
	CacheKeyspace
	ConfigLoad
	Secrets

	// API Framework Resources
	APIEndpoint
	AuthHandler
	Middleware
	ServiceStruct
)

type Resource interface {
	// Kind is the kind of resource this is.
	Kind() Kind

	// Package is the package the resource is declared in.
	Package() *pkginfo.Package
}

type Named interface {
	Resource

	// ResourceName is the name of the resource.
	ResourceName() string
}