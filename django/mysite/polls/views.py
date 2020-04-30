from django.shortcuts import render
from django.http import HttpResponse, HttpRequest

from .models import Question


def index(request: HttpRequest):
    latest_questions = Question.objects.order_by('-pub_date')[:5]
    context = {'latest_questions': latest_questions}
    return render(request, 'polls/index.html', context)


def detail(request: HttpRequest, question_id: int):
    return HttpResponse("You're looking at question: %s." % question_id)


def results(request: HttpRequest, question_id: int):
    return HttpResponse("You're looking at the results of question: %s."
                        % question_id)


def vote(request: HttpRequest, question_id: int):
    return HttpResponse("You're voting on question: %s" % question_id)
