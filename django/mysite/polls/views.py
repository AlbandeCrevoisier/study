from django.shortcuts import render, get_object_or_404
from django.http import HttpResponse, HttpRequest, Http404

from .models import Question


def index(request: HttpRequest):
    latest_questions = Question.objects.order_by('-pub_date')[:5]
    context = {'latest_questions': latest_questions}
    return render(request, 'polls/index.html', context)


def detail(request: HttpRequest, question_id: int):
    q = get_object_or_404(Question, pk=question_id)
    return render(request, 'polls/detail.html', {'question': q})


def results(request: HttpRequest, question_id: int):
    return HttpResponse("You're looking at the results of question: %s."
                        % question_id)


def vote(request: HttpRequest, question_id: int):
    return HttpResponse("You're voting on question: %s" % question_id)
