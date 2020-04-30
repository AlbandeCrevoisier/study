from django.shortcuts import render
from django.http import HttpResponse, HttpRequest, Http404

from .models import Question


def index(request: HttpRequest):
    latest_questions = Question.objects.order_by('-pub_date')[:5]
    context = {'latest_questions': latest_questions}
    return render(request, 'polls/index.html', context)


def detail(request: HttpRequest, question_id: int):
    try:
        q = Question.objects.get(pk=question_id)
    except Question.DoesNotExist:
        raise Http404("Question does not exist.")
    return render(request, 'polls/detail.html', {'question': q})


def results(request: HttpRequest, question_id: int):
    return HttpResponse("You're looking at the results of question: %s."
                        % question_id)


def vote(request: HttpRequest, question_id: int):
    return HttpResponse("You're voting on question: %s" % question_id)
