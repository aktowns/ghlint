package main

import (
	"context"
	"encoding/json"
	"github.com/google/go-github/v28/github"
	"golang.org/x/oauth2"
	"log"
	"os"
	"os/exec"
)

// [{"module":["Analyzers.Graphviz"],"decl":["ppStmt"],"severity":"Suggestion","hint":"Reduce duplication","file":"/home/ashleyis/Development/projects/osdev/radon/src/Analyzers/Graphviz.hs","startLine":102,"startColumn":3,"endLine":102,"endColumn":15,"from":"i <- freshId\nj <- freshId\n(k, ty) <- ppType t\n","to":"Combine with /home/ashleyis/Development/projects/osdev/radon/src/Analyzers/Graphviz.hs:112:3","note":[],"refactorings":"[]"}

type LintMsg struct {
	Module      []string
	Decl        []string
	Severity    string
	Hint        string
	File        string
	StartLine   int
	StartColumn int
	EndLine     int
	EndColumn   int
	From        string
	To          string
}

type GitHubEvent struct {
	PullRequest GitHubPullRequest `json:"pull_request"`
	Repository  struct {
		Owner struct {
			Login string `json:"login"`
		} `json:"owner"`
		Name string `json:"name"`
	} `json:"repository"`
	CheckSuite struct {
		After        string              `json:"after"`
		PullRequests []GitHubPullRequest `json:"pull_requests"`
	} `json:"check_suite"`
	HeadCommit struct {
		ID string `json:"id"`
	} `json:"head_commit"`
}

type GitHubPullRequest struct {
	Number int `json:"number"`
	Head   struct {
		Sha string `json:"sha"`
		Ref string `json:"ref"`
	} `json:"head"`
}

func main() {
	if len(os.Args) == 2 {
		log.Fatal("Directory is required")
	}

	cmd := exec.Command(os.Args[1], "--no-exit-code", "--json", os.Args[2])
	stdoutStderr, err := cmd.CombinedOutput()
	if err != nil {
		log.Printf("Running %s failed:\n%s", os.Args[1], stdoutStderr)
		log.Fatal(err)
	}

	var hints []LintMsg
	err = json.Unmarshal(stdoutStderr, &hints)
	if err != nil {
		log.Printf("Could not decode hlint output %+V", stdoutStderr)
		log.Fatal(err)
	}

	reviewPr(hints)
}

func reviewPr(hints []LintMsg) {
	var (
		token      = os.Getenv("INPUT_TOKEN")
		event      = os.Getenv("GITHUB_EVENT_PATH")
	)

	fh, err := os.Open(event)
	if err != nil {
		log.Fatal(err)
	}
	defer fh.Close()
	var ghevent GitHubEvent
	err = json.NewDecoder(fh).Decode(&ghevent)
	if err != nil {
		log.Fatal(err)
	}

	ctx := context.Background()
	ts := oauth2.StaticTokenSource(&oauth2.Token{AccessToken: token})
	tc := oauth2.NewClient(ctx, ts)

	client := github.NewClient(tc)

	comments := make([]*github.DraftReviewComment, 0, len(hints))
	for _, hint := range hints {
		comments = append(comments, &github.DraftReviewComment{
			Path:     nil,
			Position: nil,
			Body:     github.String(hint.Hint),
		})
	}
	review := &github.PullRequestReviewRequest{
		CommitID: github.String(ghevent.PullRequest.Head.Sha),
		Body:     github.String("testing"),
		Event:    github.String("COMMENT"),
		Comments: comments,
	}

	client.PullRequests.CreateReview(ctx, ghevent.Repository.Owner.Login, ghevent.Repository.Name, ghevent.PullRequest.Number, review)
}
