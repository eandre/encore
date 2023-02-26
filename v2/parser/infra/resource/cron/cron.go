package cron

import (
	"fmt"
	"go/ast"
	"sort"

	cronparser "github.com/robfig/cron/v3"

	"encr.dev/v2/internal/pkginfo"
	literals2 "encr.dev/v2/parser/infra/internal/literals"
	"encr.dev/v2/parser/infra/internal/locations"
	parseutil2 "encr.dev/v2/parser/infra/internal/parseutil"
	"encr.dev/v2/parser/infra/resource"
)

type Job struct {
	Name     string // The unique name of the cron job
	Doc      string // The documentation on the cron job
	Title    string // cron job title
	Schedule string
}

func (t *Job) Kind() resource.Kind { return resource.CronJob }

var JobParser = &resource.Parser{
	Name:      "Cron Job",
	DependsOn: nil,

	RequiredImports: []string{"encore.dev/cron"},
	Run: func(p *resource.Pass) []resource.Resource {
		name := pkginfo.QualifiedName{PkgPath: "encore.dev/cron", Name: "NewJob"}

		spec := &parseutil2.ResourceCreationSpec{
			AllowedLocs: locations.AllowedIn(locations.Variable).ButNotIn(locations.Function, locations.FuncCall),
			MinTypeArgs: 0,
			MaxTypeArgs: 0,
			Parse:       parseCronJob,
		}

		var resources []resource.Resource
		parseutil2.FindPkgNameRefs(p.Pkg, []pkginfo.QualifiedName{name}, func(file *pkginfo.File, name pkginfo.QualifiedName, stack []ast.Node) {
			r := parseutil2.ParseResourceCreation(p, spec, parseutil2.ReferenceData{
				File:         file,
				Stack:        stack,
				ResourceFunc: name,
			})
			if r != nil {
				resources = append(resources, r)
			}
		})
		return resources
	},
}

const (
	minute int64 = 60
	hour         = 60 * minute
)

var cronjobParser = cronparser.NewParser(cronparser.Minute | cronparser.Hour | cronparser.Dom | cronparser.Month | cronparser.Dow)

func parseCronJob(d parseutil2.ParseData) resource.Resource {
	displayName := d.ResourceFunc.NaiveDisplayName()
	if len(d.Call.Args) != 2 {
		d.Pass.Errs.Addf(d.Call.Pos(), "%s expects 2 arguments", displayName)
		return nil
	}

	jobName := parseutil2.ParseResourceName(d.Pass.Errs, displayName, "cron job name",
		d.Call.Args[0], parseutil2.KebabName, "")
	if jobName == "" {
		// we already reported the error inside ParseResourceName
		return nil
	}

	cfgLit, ok := literals2.ParseStruct(d.Pass.Errs, d.File, "cron.JobConfig", d.Call.Args[1])
	if !ok {
		return nil // error reported by ParseStruct
	}

	// Decode the config
	type decodedConfig struct {
		Title    string   `literal:",optional"`
		Endpoint ast.Expr `literal:",required,dynamic"`
		Every    int64    `literal:",optional"`
		Schedule string   `literal:",optional"`
	}
	config := literals2.Decode[decodedConfig](d.Pass.Errs, cfgLit)

	job := &Job{
		Name:  jobName,
		Doc:   d.Doc,
		Title: config.Title,
	}
	if job.Title == "" {
		job.Title = jobName
	}

	// Parse the schedule
	switch {
	case config.Every != 0 && config.Schedule != "":
		d.Pass.Errs.Addf(cfgLit.Pos("Every"), "cron execution schedule was set twice, once in Every and one in Schedule, at least one must be set but not both")
		return nil
	case config.Schedule != "":
		_, err := cronjobParser.Parse(config.Schedule)
		if err != nil {
			d.Pass.Errs.Addf(cfgLit.Pos("Schedule"), "Schedule must be a valid cron expression: %s", err)
			return nil
		}
		job.Schedule = fmt.Sprintf("schedule:%s", config.Schedule)
	case config.Every != 0:
		if rem := config.Every % minute; rem != 0 {
			d.Pass.Errs.Addf(cfgLit.Pos("Every"), "Every: must be an integer number of minutes, got %d", config.Every)
			return nil
		}

		minutes := config.Every / minute
		if minutes < 1 {
			d.Pass.Errs.Addf(cfgLit.Pos("Every"), "Every: duration must be one minute or greater, got %d", minutes)
			return nil
		} else if minutes > 24*60 {
			d.Pass.Errs.Addf(cfgLit.Pos("Every"), "Every: duration must not be greater than 24 hours (1440 minutes), got %d", minutes)
			return nil
		} else if suggestion, ok := isCronIntervalAllowed(int(minutes)); !ok {
			suggestionStr := formatMinutes(suggestion)
			minutesStr := formatMinutes(int(minutes))
			d.Pass.Errs.Addf(cfgLit.Pos("Every"), "Every: 24 hour time range (from 00:00 to 23:59) "+
				"needs to be evenly divided by the interval value (%s), try setting it to (%s)", minutesStr, suggestionStr)
			return nil
		}
		job.Schedule = fmt.Sprintf("every:%d", minutes)
	}

	return job
}

// abs returns the absolute value of x.
func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func formatMinutes(minutes int) string {
	if minutes < 60 {
		return fmt.Sprintf("%d * cron.Minute", minutes)
	} else if minutes%60 == 0 {
		return fmt.Sprintf("%d * cron.Hour", minutes/60)
	}
	return fmt.Sprintf("%d * cron.Hour + %d * cron.Minute", minutes/60, minutes%60)
}

func isCronIntervalAllowed(val int) (suggestion int, ok bool) {
	allowed := []int{
		1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 30, 32, 36, 40, 45,
		48, 60, 72, 80, 90, 96, 120, 144, 160, 180, 240, 288, 360, 480, 720, 1440,
	}
	idx := sort.SearchInts(allowed, val)

	if idx == len(allowed) {
		return allowed[len(allowed)-1], false
	} else if allowed[idx] == val {
		return val, true
	} else if idx == 0 {
		return allowed[0], false
	} else if abs(val-allowed[idx-1]) < abs(val-allowed[idx]) {
		return allowed[idx-1], false
	}

	return allowed[idx], false
}