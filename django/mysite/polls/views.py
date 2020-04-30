from django.shortcuts import render
from django.http import HttpResponse, HttpRequest


def index(request: HttpRequest):
    return HttpResponse("Hello, world. You're at the polls index.")


def detail(request: HttpRequest, question_id: int):
    return HttpResponse("You're looking at question: %s." % question_id)


def results(request: HttpRequest, question_id: int):
    return HttpResponse("You're looking at the results of question: %s."
                        % question_id)


def vote(request: HttpRequest, question_id: int):
    return HttpResponse("You're voting on question: %s" % question_id)
